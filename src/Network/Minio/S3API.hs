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
import Data.Default (def)

import           Lib.Prelude

import           Network.Minio.Data
import Network.Minio.API
import Network.Minio.XmlParser
import Network.Minio.XmlGenerator


-- | Fetch all buckets from the service.
getService :: Minio [BucketInfo]
getService = do
  resp <- executeRequest $ def
  parseListBuckets $ NC.responseBody resp

-- | Fetch bucket location (region)
getLocation :: Bucket -> Minio Text
getLocation bucket = do
  resp <- executeRequest $ def { riBucket = Just bucket
                               , riQueryParams = [("location", Nothing)]
                               }
  parseLocation $ NC.responseBody resp

-- | GET an object from the service and return the response headers
-- and a conduit source for the object content
getObject :: Bucket -> Object -> HT.Query -> [HT.Header]
          -> Minio ([HT.Header], C.ResumableSource Minio ByteString)
getObject bucket object queryParams headers = do
  resp <- mkStreamRequest reqInfo
  return $ (NC.responseHeaders resp, NC.responseBody resp)
  where
    reqInfo = def { riBucket = Just bucket
                  , riObject = Just object
                  , riQueryParams = queryParams
                  , riHeaders = headers}

-- | Creates a bucket via a PUT bucket call.
putBucket :: Bucket -> Location -> Minio ()
putBucket bucket location = do
  void $ executeRequest $
    def { riMethod = HT.methodPut
        , riBucket = Just bucket
        , riPayload = PayloadBS $ mkCreateBucketConfig location
        }

-- | Single PUT object size.
maxSinglePutObjectSizeBytes :: Int64
maxSinglePutObjectSizeBytes = 5 * 1024 * 1024 * 1024

-- | PUT an object into the service. This function performs a single
-- PUT object call, and so can only transfer objects upto 5GiB.
putObject :: Bucket -> Object -> [HT.Header] -> Int64
          -> Int64 -> Handle -> Minio ()
putObject bucket object headers offset size h = do
  -- check length is within single PUT object size.
  when (size > maxSinglePutObjectSizeBytes) $
    throwError $ MErrValidation $ MErrVSinglePUTSizeExceeded size

  -- content-length header is automatically set by library.
  void $ executeRequest $
    def { riMethod = HT.methodPut
        , riBucket = Just bucket
        , riObject = Just object
        , riHeaders = headers
        , riPayload = PayloadH h offset size
        }

-- | DELETE a bucket from the service.
deleteBucket :: Bucket -> Minio ()
deleteBucket bucket = do
  void $ executeRequest $
    def { riMethod = HT.methodDelete
        , riBucket = Just bucket
        }

-- | DELETE an object from the service.
deleteObject :: Bucket -> Object -> Minio ()
deleteObject bucket object = do
  void $ executeRequest $
    def { riMethod = HT.methodDelete
        , riBucket = Just bucket
        , riObject = Just object
        }
