import           Test.QuickCheck (generate)
import qualified Test.QuickCheck as Q
import           Test.Tasty
import           Test.Tasty.HUnit

import           Lib.Prelude

import qualified System.IO as SIO

import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Conduit (($$))
import           Data.Conduit.Combinators (sinkList)
import           Data.Default (Default(..))
import qualified Data.Text as T

import           Network.Minio
import           Network.Minio.Data
import           Network.Minio.S3API
import           Network.Minio.XmlGenerator.Test
import           Network.Minio.XmlParser.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests, liveServerUnitTests]

properties :: TestTree
properties = testGroup "Properties" [] -- [scProps, qcProps]

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

-- qcProps = testGroup "(checked by QuickCheck)"
--   [ QC.testProperty "sort == sort . reverse" $
--       \list -> sort (list :: [Int]) == sort (reverse list)
--   , QC.testProperty "Fermat's little theorem" $
--       \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--   -- the following property does not hold
--   , QC.testProperty "Fermat's last theorem" $
--       \x y z n ->
--         (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
--   ]

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
    putBucket b "us-east-1"
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

      step "simple getObject works"
      fGetObject bucket "lsb-release" "/tmp/out"

      step "create new multipart upload works"
      uid <- newMultipartUpload bucket "newmpupload" []
      liftIO $ (T.length uid > 0) @? ("Got an empty multipartUpload Id.")

      step "abort a new multipart upload works"
      abortMultipartUpload bucket "newmpupload" uid

      step "delete object works"
      deleteObject bucket "lsb-release"

  , funTestWithBucket "Multipart Upload Test" $ \step bucket -> do
      let object = "newmpupload"

      step "create new multipart upload"
      uid <- newMultipartUpload bucket object []
      liftIO $ (T.length uid > 0) @? ("Got an empty multipartUpload Id.")

      step "put object parts 1..10"
      h <- liftIO $ SIO.openBinaryFile "/tmp/inputfile" SIO.ReadMode
      let mb15 = 15 * 1024 * 1024
      partInfo <- forM [1..10] $ \pnum ->
        putObjectPart bucket object uid pnum [] $ PayloadH h 0 mb15

      step "complete multipart"
      etag <- completeMultipartUpload bucket object uid partInfo

      step $ "completeMultipart success - got etag: " ++ show etag

      step $ "Retrieve the created object"
      fGetObject bucket object "/tmp/newUpload"

      step $ "Cleanup actions"
      deleteObject bucket object

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
      -- fPutObject bucket "big" "/tmp/large"
      -- putObject bucket "big" ("/dev/zero")
      etag <- putObject bucket "big" (ODFile "/dev/zero" $ Just $ 1024*1024*100)
      traceShowM etag

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
      h <- liftIO $ SIO.openBinaryFile "/tmp/inputfile" SIO.ReadMode
      forM [1..10] $ \pnum ->
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
      h <- liftIO $ SIO.openBinaryFile "/tmp/inputfile" SIO.ReadMode
      forM [1..10] $ \pnum ->
        putObjectPart bucket object uid pnum [] $ PayloadH h 0 mb15

      step "fetch list parts"
      incompleteParts <- (listIncompleteParts bucket object uid) $$ sinkList
      liftIO $ (length incompleteParts) @?= 10

  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [xmlGeneratorTests, xmlParserTests]
