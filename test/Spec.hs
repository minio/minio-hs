import qualified Test.QuickCheck as Q
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Lib.Prelude

import           System.Directory (getTemporaryDirectory)
import qualified System.IO as SIO

import qualified Control.Monad.Trans.Resource as R
import qualified Data.ByteString as BS
import           Data.Conduit (($$), yield)
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import           Data.Conduit.Combinators (sinkList)
import           Data.Default (Default(..))
import qualified Data.Text as T
import qualified Data.List as L

import           Network.Minio
import           Network.Minio.Data
import           Network.Minio.PutObject
import           Network.Minio.S3API
import           Network.Minio.Utils
import           Network.Minio.XmlGenerator.Test
import           Network.Minio.XmlParser.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests, liveServerUnitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps] -- [scProps]

-- scProps = testGroup "(checked by SmallCheck)"
--   [ SC.testProperty "sort == sort . reverse" $
--       \list -> sort (list :: [Int]) == sort (reverse list)
--   , SC.testProperty "Fermat's little theorem" $
--       \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--   -- the following property does not hold
--   , SC.testProperty "Fermat's last theorem" $
--       \x y z n ->
--         (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
--   ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "selectPartSizes: simple properties" $
    \n -> let (pns, offs, sizes) = L.unzip3 (selectPartSizes n)

              -- check that pns increments from 1.
              isPNumsAscendingFrom1 = all (\(a, b) -> a == b) $ zip pns [1..]

              consPairs [] = []
              consPairs [_] = []
              consPairs (a:(b:c)) = (a, b):(consPairs (b:c))

              -- check `offs` is monotonically increasing.
              isOffsetsAsc = all (\(a, b) -> a < b) $ consPairs offs

              -- check sizes sums to n.
              isSumSizeOk = n < 0 || (sum sizes == n && all (> 0) sizes)

              -- check sizes are constant except last
              isSizesConstantExceptLast =
                n <= 0 || all (\(a, b) -> a == b) (consPairs $ L.init sizes)

          in isPNumsAscendingFrom1 && isOffsetsAsc && isSumSizeOk &&
             isSizesConstantExceptLast

  , QC.testProperty "selectPartSizes: part-size is at least 64MiB" $
    \n -> let (_, _, sizes) = L.unzip3 (selectPartSizes n)
              mib64 = 64 * 1024 * 1024
          in if | length sizes > 1 -> -- last part can be smaller but > 0
                    all (>= mib64) (L.init sizes) && L.last sizes > 0
                | length sizes == 1 -> maybe True (> 0) $ head sizes
                | otherwise -> True
  ]


-- conduit that generates random binary stream of given length
randomDataSrc :: MonadIO m => Int64 -> C.Producer m ByteString
randomDataSrc s' = genBS s'
  where
    oneMiB = 1024*1024

    concatIt bs n = BS.concat $ replicate (fromIntegral q) bs ++
                    [BS.take (fromIntegral r) bs]
      where (q, r) = n `divMod` fromIntegral (BS.length bs)

    genBS s = do
      w8s <- liftIO $ generate $ Q.vectorOf 64 (Q.choose (0, 255))
      let byteArr64 = BS.pack w8s
      if s < oneMiB
        then yield $ concatIt byteArr64 s
        else do yield $ concatIt byteArr64 oneMiB
                genBS (s - oneMiB)

mkRandFile :: R.MonadResource m => Int64 -> m FilePath
mkRandFile size = do
  dir <- liftIO $ getTemporaryDirectory
  randomDataSrc size C.$$ CB.sinkTempFile dir "miniohstest.random"

funTestBucketPrefix :: Text
funTestBucketPrefix = "miniohstest-"

funTestWithBucket :: TestName
                  -> (([Char] -> Minio ()) -> Bucket -> Minio ()) -> TestTree
funTestWithBucket t minioTest = testCaseSteps t $ \step -> do
  -- generate a random name for the bucket
  bktSuffix <- liftIO $ generate $ Q.vectorOf 10 (Q.choose ('a', 'z'))
  let b = T.concat [funTestBucketPrefix, T.pack bktSuffix]
      liftStep = liftIO . step
  ret <- runResourceT $ runMinio def $ do
    liftStep $ "Creating bucket for test - " ++ t
    makeBucket b def
    minioTest liftStep b
    deleteBucket b
  isRight ret @? ("Functional test " ++ t ++ " failed => " ++ show ret)

liveServerUnitTests :: TestTree
liveServerUnitTests = testGroup "Unit tests against a live server"
  [ funTestWithBucket "Basic tests" $ \step bucket -> do
      step "getService works and contains the test bucket."
      buckets <- getService
      unless (length (filter (== bucket) $ map biName buckets) == 1) $
        liftIO $
        assertFailure ("The bucket " ++ show bucket ++
                       " was expected to exist.")

      step "getLocation works"
      region <- getLocation bucket
      liftIO $ region == "" @? ("Got unexpected region => " ++ show region)

      step "singlepart putObject works"
      fPutObject bucket "lsb-release" "/etc/lsb-release"

      outFile <- mkRandFile 0
      step "simple getObject works"
      fGetObject bucket "lsb-release" outFile

      step "create new multipart upload works"
      uid <- newMultipartUpload bucket "newmpupload" []
      liftIO $ (T.length uid > 0) @? ("Got an empty multipartUpload Id.")

      step "abort a new multipart upload works"
      abortMultipartUpload bucket "newmpupload" uid

      step "delete object works"
      deleteObject bucket "lsb-release"

  , funTestWithBucket "Basic Multipart Test" $ \step bucket -> do
      let object = "newmpupload"

      step "create new multipart upload"
      uid <- newMultipartUpload bucket object []
      liftIO $ (T.length uid > 0) @? ("Got an empty multipartUpload Id.")

      let mb15 = 15 * 1024 * 1024
      randFile <- mkRandFile mb15

      step "put object parts 1 of 1"
      h <- liftIO $ SIO.openBinaryFile randFile SIO.ReadMode
      partInfo <- putObjectPart bucket object uid 1 [] $ PayloadH h 0 mb15

      step "complete multipart"
      void $ completeMultipartUpload bucket object uid [partInfo]

      destFile <- mkRandFile 0
      step $ "Retrieve the created object and check size"
      fGetObject bucket object destFile
      gotSize <- withNewHandle destFile getFileSize
      liftIO $ gotSize == Right (Just mb15) @?
        "Wrong file size of put file after getting"

      step $ "Cleanup actions"
      deleteObject bucket object

  , funTestWithBucket "Multipart test with unknown object size" $
    \step bucket -> do
      let obj = "mpart"

      step "Prepare"
      let mb100 = 100 * 1024 * 1024
      rFile <- mkRandFile mb100

      step "Upload multipart file."
      putObjectFromSource bucket obj (CB.sourceFile rFile) Nothing

      step "Retrieve and verify file size"
      destFile <- mkRandFile 0
      fGetObject bucket obj destFile
      gotSize <- withNewHandle destFile getFileSize
      liftIO $ gotSize == Right (Just mb100) @?
        "Wrong file size of put file after getting"

      step $ "Cleanup actions"
      deleteObject bucket obj

  , funTestWithBucket "Multipart test with non-seekable file" $
    \step bucket -> do
      let obj = "mpart"
          mb100 = 100 * 1024 * 1024

      step "Upload multipart file."
      void $ putObject bucket obj $ ODFile "/dev/zero" (Just mb100)

      step "Retrieve and verify file size"
      destFile <- mkRandFile 0
      fGetObject bucket obj destFile
      gotSize <- withNewHandle destFile getFileSize
      liftIO $ gotSize == Right (Just mb100) @?
        "Wrong file size of put file after getting"

      step $ "Cleanup actions"
      deleteObject bucket obj

  , funTestWithBucket "Basic listObjects Test" $ \step bucket -> do
      step "put 10 objects"
      forM_ [1..10::Int] $ \s ->
        fPutObject bucket (T.concat ["lsb-release", T.pack (show s)]) "/etc/lsb-release"

      step "Simple list"
      res <- listObjects' bucket Nothing Nothing Nothing
      let expected = sort $ map (T.concat .
                          ("lsb-release":) .
                          (\x -> [x]) .
                          T.pack .
                          show) [1..10::Int]
      liftIO $ assertEqual "Objects match failed!" expected
        (map oiObject $ lorObjects res)

      step "Cleanup actions"
      forM_ [1..10::Int] $ \s -> deleteObject bucket (T.concat ["lsb-release", T.pack (show s)])

  , funTestWithBucket "Basic listMultipartUploads Test" $ \step bucket -> do
      let object = "newmpupload"
      step "create 10 multipart uploads"
      forM_ [1..10::Int] $ \_ -> do
        uid <- newMultipartUpload bucket object []
        liftIO $ (T.length uid > 0) @? ("Got an empty multipartUpload Id.")

      step "list incomplete multipart uploads"
      incompleteUploads <- listIncompleteUploads' bucket Nothing Nothing Nothing Nothing
      liftIO $ (length $ lurUploads incompleteUploads) @?= 10

  , funTestWithBucket "multipart" $ \step bucket -> do

      step "upload large object"
      void $ putObject bucket "big" (ODFile "/dev/zero" $ Just $ 1024*1024*100)

      step "cleanup"
      deleteObject bucket "big"

  , funTestWithBucket "Basic listIncompleteParts Test" $ \step bucket -> do
      let
        object = "newmpupload"
        mb15 = 15 * 1024 * 1024

      step "create a multipart upload"
      uid <- newMultipartUpload bucket object []
      liftIO $ (T.length uid > 0) @? ("Got an empty multipartUpload Id.")

      step "put object parts 1..10"
      inputFile <- mkRandFile mb15
      h <- liftIO $ SIO.openBinaryFile inputFile SIO.ReadMode
      forM_ [1..10] $ \pnum ->
        putObjectPart bucket object uid pnum [] $ PayloadH h 0 mb15

      step "fetch list parts"
      listPartsResult <- listIncompleteParts' bucket object uid Nothing Nothing
      liftIO $ (length $ lprParts listPartsResult) @?= 10

  , funTestWithBucket "High-level listObjects Test" $ \step bucket -> do
      step "put 3 objects"
      let expected = [
              "dir/o1"
            , "dir/dir1/o2"
            , "dir/dir2/o3"
            ]
      forM_ expected $
        \obj -> fPutObject bucket obj "/etc/lsb-release"

      step "High-level listing of objects"
      objects <- (listObjects bucket Nothing True) $$ sinkList

      liftIO $ assertEqual "Objects match failed!" (sort expected)
        (map oiObject objects)

      step "Cleanup actions"
      forM_ expected $
        \obj -> deleteObject bucket obj

  , funTestWithBucket "High-level listIncompleteUploads Test" $ \step bucket -> do
      let object = "newmpupload"
      step "create 10 multipart uploads"
      forM_ [1..10::Int] $ \_ -> do
        uid <- newMultipartUpload bucket object []
        liftIO $ (T.length uid > 0) @? ("Got an empty multipartUpload Id.")

      step "High-level listing of incomplete multipart uploads"
      uploads <- (listIncompleteUploads bucket Nothing True) $$ sinkList

      liftIO $ (length uploads) @?= 10

  , funTestWithBucket "High-level listIncompleteParts Test" $ \step bucket -> do
      let
        object = "newmpupload"
        mb15 = 15 * 1024 * 1024

      step "create a multipart upload"
      uid <- newMultipartUpload bucket object []
      liftIO $ (T.length uid > 0) @? ("Got an empty multipartUpload Id.")

      step "put object parts 1..10"
      inputFile <- mkRandFile mb15
      h <- liftIO $ SIO.openBinaryFile inputFile SIO.ReadMode
      forM_ [1..10] $ \pnum ->
        putObjectPart bucket object uid pnum [] $ PayloadH h 0 mb15

      step "fetch list parts"
      incompleteParts <- (listIncompleteParts bucket object uid) $$ sinkList
      liftIO $ (length incompleteParts) @?= 10

  , funTestWithBucket "High-level statObject Test" $ \step bucket -> do
      let
        object = "sample"
        zeroByte = 0

      step "create an object"
      inputFile <- mkRandFile zeroByte
      fPutObject bucket object inputFile

      step "get metadata of the object"
      res <- statObject bucket object
      liftIO $ (oiSize res) @?= 0

      step "delete object"
      deleteObject bucket object
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [xmlGeneratorTests, xmlParserTests]
