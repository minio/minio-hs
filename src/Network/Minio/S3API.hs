module Network.Minio.S3API
  ( getService
  , getLocation
  , getObject
  , putBucket
  , putObject
  , deleteBucket
  , deleteObject
  ) where

import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Conduit as NC
import qualified Data.Conduit as C
-- import Control.Monad.Trans.Resource (MonadResource)
-- import Data.Conduit.Binary (sinkLbs, sourceHandleRange)
-- import qualified Data.ByteString.Lazy as LB

import           Lib.Prelude

import           Network.Minio.Data
import Network.Minio.API
import Network.Minio.XmlParser
import Network.Minio.XmlGenerator


getService :: Minio [BucketInfo]
getService = do
  resp <- executeRequest $
    requestInfo HT.methodGet Nothing Nothing [] [] EPayload
  parseListBuckets $ NC.responseBody resp

getLocation :: Bucket -> Minio Text
getLocation bucket = do
  resp <- executeRequest $
    requestInfo HT.methodGet (Just bucket) Nothing [("location", Nothing)] []
    EPayload
  parseLocation $ NC.responseBody resp

getObject :: Bucket -> Object -> HT.Query -> [HT.Header]
          -> Minio ([HT.Header], C.ResumableSource Minio ByteString)
getObject bucket object queryParams headers = do
  resp <- mkStreamRequest reqInfo
  return $ (NC.responseHeaders resp, NC.responseBody resp)
  where
    reqInfo = requestInfo HT.methodGet (Just bucket) (Just object)
              queryParams headers EPayload

putBucket :: Bucket -> Location -> Minio ()
putBucket bucket location = do
  void $ executeRequest $
    requestInfo HT.methodPut (Just bucket) Nothing [] [] $
    PayloadBS $ mkCreateBucketConfig location

maxSinglePutObjectSizeBytes :: Int64
maxSinglePutObjectSizeBytes = 5 * 1024 * 1024 * 1024

putObject :: Bucket -> Object -> [HT.Header] -> Int64
          -> Int64 -> Handle -> Minio ()
putObject bucket object headers offset size h = do
  -- check length is within single PUT object size.
  when (size > maxSinglePutObjectSizeBytes) $
    throwError $ MErrValidation $ MErrVSinglePUTSizeExceeded size

  -- content-length header is automatically set by library.
  void $ executeRequest $
    requestInfo HT.methodPut (Just bucket) (Just object) [] headers $
    PayloadH h offset size


deleteBucket :: Bucket -> Minio ()
deleteBucket bucket = do
  void $ executeRequest $
    requestInfo HT.methodDelete (Just bucket) Nothing [] [] EPayload

deleteObject :: Bucket -> Object -> Minio ()
deleteObject bucket object = do
  void $ executeRequest $
    requestInfo HT.methodDelete (Just bucket) (Just object) [] [] EPayload
