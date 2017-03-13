--
-- Minio Haskell SDK, (C) 2017 Minio, Inc.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

import qualified Test.QuickCheck as Q
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Lib.Prelude

import           System.Directory (getTemporaryDirectory)
import qualified System.IO as SIO

import qualified Control.Monad.Catch as MC
import qualified Control.Monad.Trans.Resource as R
import qualified Data.ByteString as BS
import           Data.Conduit (($$), yield)
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import           Data.Conduit.Combinators (sinkList)
import           Data.Default (Default(..))
import qualified Data.Text as T
import           System.Environment (lookupEnv)

import           Network.Minio
import           Network.Minio.Data
import           Network.Minio.Errors
import           Network.Minio.ListOps
import           Network.Minio.PutObject
import           Network.Minio.S3API
import           Network.Minio.Utils

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [liveServerUnitTests]

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
  connInfo <- maybe minioPlayCI (const def) <$> lookupEnv "MINIO_LOCAL"
  ret <- runResourceT $ runMinio connInfo $ do
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

      step "makeBucket again to check if BucketAlreadyOwnedByYou exception is raised."
      mbE <- MC.try $ makeBucket bucket Nothing
      case mbE of
        Left exn -> liftIO $ exn @?= (MErrService BucketAlreadyOwnedByYou)
        _ -> return ()

      step "makeBucket with an invalid bucket name and check for appropriate exception."
      invalidMBE <- MC.try $ makeBucket "invalidBucketName" Nothing
      case invalidMBE of
        Left exn -> liftIO $ exn @?= (MErrService InvalidBucketName)
        _ -> return ()

      step "getLocation works"
      region <- getLocation bucket
      liftIO $ region == "us-east-1" @? ("Got unexpected region => " ++ show region)

      step "singlepart putObject works"
      fPutObject bucket "lsb-release" "/etc/lsb-release"

      step "fPutObject onto a non-existent bucket and check for NoSuchBucket exception"
      fpE <- MC.try $ fPutObject "nosuchbucket" "lsb-release" "/etc/lsb-release"
      case fpE of
        Left exn -> liftIO $ exn @?= (MErrService NoSuchBucket)
        _ -> return ()

      outFile <- mkRandFile 0
      step "simple fGetObject works"
      fGetObject bucket "lsb-release" outFile

      step "fGetObject a non-existent object and check for NoSuchKey exception"
      resE <- MC.try $ fGetObject bucket "noSuchKey" outFile
      case resE of
        Left exn -> liftIO $ exn @?= (MErrService NoSuchKey)
        _ -> return ()


      step "create new multipart upload works"
      uid <- newMultipartUpload bucket "newmpupload" []
      liftIO $ (T.length uid > 0) @? ("Got an empty multipartUpload Id.")

      step "abort a new multipart upload works"
      abortMultipartUpload bucket "newmpupload" uid

      step "delete object works"
      deleteObject bucket "lsb-release"

      step "statObject test"
      let object = "sample"
      step "create an object"
      inputFile <- mkRandFile 0
      fPutObject bucket object inputFile

      step "get metadata of the object"
      res <- statObject bucket object
      liftIO $ (oiSize res) @?= 0

      step "delete object"
      deleteObject bucket object

  , funTestWithBucket "Multipart Tests" $
    \step bucket -> do
      -- low-level multipart operation tests.
      let object = "newmpupload"
          mb15 = 15 * 1024 * 1024

      step "Prepare for low-level multipart tests."
      step "create new multipart upload"
      uid <- newMultipartUpload bucket object []
      liftIO $ (T.length uid > 0) @? ("Got an empty multipartUpload Id.")

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
      removeObject bucket object

      -- putObject test (conduit source, no size specified)
      let obj = "mpart"
          mb100 = 100 * 1024 * 1024

      step "Prepare for putObject with from source without providing size."
      rFile <- mkRandFile mb100

      step "Upload multipart file."
      putObject bucket obj (CB.sourceFile rFile) Nothing

      step "Retrieve and verify file size"
      destFile <- mkRandFile 0
      fGetObject bucket obj destFile
      gotSize <- withNewHandle destFile getFileSize
      liftIO $ gotSize == Right (Just mb100) @?
        "Wrong file size of put file after getting"

      step $ "Cleanup actions"
      deleteObject bucket obj

      step "Prepare for putObjectInternal with non-seekable file, with size."
      step "Upload multipart file."
      void $ putObjectInternal bucket obj $ ODFile "/dev/zero" (Just mb100)

      step "Retrieve and verify file size"
      destFile <- mkRandFile 0
      fGetObject bucket obj destFile
      gotSize <- withNewHandle destFile getFileSize
      liftIO $ gotSize == Right (Just mb100) @?
        "Wrong file size of put file after getting"

      step $ "Cleanup actions"
      removeObject bucket obj

      step "Prepare for putObjectInternal with large file as source."
      step "upload large object"
      void $ putObjectInternal bucket "big" (ODFile "/dev/zero" $
                                             Just $ 1024*1024*100)

      step "cleanup"
      removeObject bucket "big"


  , funTestWithBucket "Listing Test" $ \step bucket -> do
      step "listObjects' test"
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

      step "listIncompleteUploads' test"
      let object = "newmpupload"
      step "create 10 multipart uploads"
      forM_ [1..10::Int] $ \_ -> do
        uid <- newMultipartUpload bucket object []
        liftIO $ (T.length uid > 0) @? ("Got an empty multipartUpload Id.")

      step "list incomplete multipart uploads"
      incompleteUploads <- listIncompleteUploads' bucket Nothing Nothing
                           Nothing Nothing
      liftIO $ (length $ lurUploads incompleteUploads) @?= 10

      step "cleanup"
      forM_ (lurUploads incompleteUploads) $
        \(_, uid, _) -> abortMultipartUpload bucket object uid

      step "Basic listIncompleteParts Test"
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
      abortMultipartUpload bucket object uid

      step "High-level listObjects Test"
      step "put 3 objects"
      let expected = ["dir/o1", "dir/dir1/o2", "dir/dir2/o3"]
      forM_ expected $
        \obj -> fPutObject bucket obj "/etc/lsb-release"

      step "High-level listing of objects"
      objects <- (listObjects bucket Nothing True) $$ sinkList

      liftIO $ assertEqual "Objects match failed!" (sort expected)
        (map oiObject objects)

      step "Cleanup actions"
      forM_ expected $
        \obj -> removeObject bucket obj

      step "High-level listIncompleteUploads Test"
      let object = "newmpupload"
      step "create 10 multipart uploads"
      forM_ [1..10::Int] $ \_ -> do
        uid <- newMultipartUpload bucket object []
        liftIO $ (T.length uid > 0) @? ("Got an empty multipartUpload Id.")

      step "High-level listing of incomplete multipart uploads"
      uploads <- (listIncompleteUploads bucket Nothing True) $$ sinkList
      liftIO $ (length uploads) @?= 10

      step "cleanup"
      forM_ uploads $ \(UploadInfo _ uid _ _) ->
                        abortMultipartUpload bucket object uid

      step "High-level listIncompleteParts Test"
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

      step "cleanup"
      abortMultipartUpload bucket object uid

  , funTestWithBucket "copyObject related tests" $ \step bucket -> do
      step "copyObjectSingle basic tests"
      let object = "xxx"
          objCopy = "xxxCopy"
          size1 = 100 :: Int64

      step "create server object to copy"
      inputFile <- mkRandFile size1
      fPutObject bucket object inputFile

      step "copy object"
      let cps = def { cpSource = format "/{}/{}" [bucket, object] }
      (etag, modTime) <- copyObjectSingle bucket objCopy cps []

      -- retrieve obj info to check
      ObjectInfo _ t e s <- headObject bucket objCopy

      let isMTimeDiffOk = abs (diffUTCTime modTime t) < 1.0

      liftIO $ (s == size1 && e == etag && isMTimeDiffOk) @?
        "Copied object did not match expected."

      step "cleanup actions"
      removeObject bucket object
      removeObject bucket objCopy

      step "copyObjectPart basic tests"
      let srcObj = "XXX"
          copyObj = "XXXCopy"

      step "Prepare"
      let mb15 = 15 * 1024 * 1024
          mb5 = 5 * 1024 * 1024
      randFile <- mkRandFile mb15
      fPutObject bucket srcObj randFile

      step "create new multipart upload"
      uid <- newMultipartUpload bucket copyObj []
      liftIO $ (T.length uid > 0) @? ("Got an empty multipartUpload Id.")

      step "put object parts 1-3"
      let cps = def {cpSource = format "/{}/{}" [bucket, srcObj]}
      parts <- forM [1..3] $ \p -> do
        (etag, _) <- copyObjectPart bucket copyObj cps{
          cpSourceRange = Just ((p-1)*mb5, (p-1)*mb5 + (mb5 - 1))
          } uid (fromIntegral p) []
        return $ (fromIntegral p, etag)

      step "complete multipart"
      void $ completeMultipartUpload bucket copyObj uid parts

      step "verify copied object size"
      (ObjectInfo _ _ _ s) <- headObject bucket copyObj

      liftIO $ (s == mb15) @? "Size failed to match"

      step $ "Cleanup actions"
      removeObject bucket srcObj
      removeObject bucket copyObj

      step "copyObject basic tests"
      let srcs = ["XXX", "XXXL"]
          copyObjs = ["XXXCopy", "XXXLCopy"]
          sizes = map (* (1024 * 1024)) [15, 65]

      step "Prepare"
      forM_ (zip srcs sizes) $ \(src, size) ->
        fPutObject bucket src =<< mkRandFile size

      step "make small and large object copy"
      forM_ (zip copyObjs srcs) $ \(cp, src) ->
        copyObject bucket cp def{cpSource = format "/{}/{}" [bucket, src]}

      step "verify uploaded objects"
      uploadedSizes <- fmap (fmap oiSize) $ forM copyObjs (headObject bucket)

      liftIO $ (sizes == uploadedSizes) @? "Uploaded obj sizes failed to match"

      forM_ (concat [srcs, copyObjs]) (removeObject bucket)

      step "copyObject with offset test "
      let src = "XXX"
          copyObj = "XXXCopy"
          size = 15 * 1024 * 1024

      step "Prepare"
      fPutObject bucket src =<< mkRandFile size

      step "copy last 10MiB of object"
      copyObject bucket copyObj def{
          cpSource = format "/{}/{}" [bucket, src]
        , cpSourceRange = Just (5 * 1024 * 1024, size - 1)
        }

      step "verify uploaded object"
      cSize <- oiSize <$> headObject bucket copyObj

      liftIO $ (cSize == 10 * 1024 * 1024) @? "Uploaded obj size mismatched!"

      forM_ [src, copyObj] (removeObject bucket)
  ]
