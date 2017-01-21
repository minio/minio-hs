module Network.Minio.S3API
  (
    getLocation

  -- * Listing buckets
  --------------------
  , getService

  -- * Listing objects
  --------------------
  , listObjects

  -- * Retrieving objects
  -----------------------
  , getObject

  -- * Creating buckets and objects
  ---------------------------------
  , putBucket
  , putObject

  -- * Deletion APIs
  --------------------------
  , deleteBucket
  , deleteObject

  -- * Multipart Upload APIs
  --------------------------
  , newMultipartUpload
  , putObjectPart
  , completeMultipartUpload
  , abortMultipartUpload
  ) where

import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Conduit as NC
import qualified Data.Conduit as C
import Data.Default (def)

import           Lib.Prelude

import           Network.Minio.Data
import Network.Minio.API
import Network.Minio.Utils
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
putBucket :: Bucket -> Region -> Minio ()
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
putObject :: Bucket -> Object -> [HT.Header] -> Handle -> Int64
          -> Int64 -> Minio ()
putObject bucket object headers h offset size = do
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

listObjects :: Bucket -> Maybe Text -> Maybe Text -> Maybe Text
            -> Minio ListObjectsResult
listObjects bucket prefix nextToken delimiter = do
  resp <- executeRequest $ def { riMethod = HT.methodGet
                               , riBucket = Just bucket
                               , riQueryParams = ("list-type", Just "2") : qp
                               }
  parseListObjectsResponse $ NC.responseBody resp
  where
    -- build optional query params
    ctokList = map ((\k -> ("continuation_token", k)) . Just . encodeUtf8) $
               maybeToList nextToken
    prefixList = map ((\k -> ("prefix", k)) . Just . encodeUtf8) $
                 maybeToList prefix
    delimList = map ((\k -> ("delimiter", k)) . Just . encodeUtf8) $
                maybeToList delimiter
    qp = concat [ctokList, prefixList, delimList]


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

-- | Create a new multipart upload.
newMultipartUpload :: Bucket -> Object -> [HT.Header] -> Minio UploadId
newMultipartUpload bucket object headers = do
  resp <- executeRequest $ def { riMethod = HT.methodPost
                               , riBucket = Just bucket
                               , riObject = Just object
                               , riQueryParams = [("uploads", Nothing)]
                               , riHeaders = headers
                               }
  parseNewMultipartUpload $ NC.responseBody resp

-- | PUT a part of an object as part of a multi-part upload.
putObjectPart :: Bucket -> Object -> UploadId -> PartNumber -> [HT.Header]
              -> Handle -> Int64 -> Int64 -> Minio PartInfo
putObjectPart bucket object uploadId partNumber headers h offset size = do
  resp <- executeRequest $
          def { riMethod = HT.methodPut
              , riBucket = Just bucket
              , riObject = Just object
              , riQueryParams = [("partNumber", Just $ encodeUtf8 $
                                                show partNumber),
                                 ("uploadId", Just $ encodeUtf8 uploadId)]
              , riHeaders = headers
              , riPayload = PayloadH h offset size
              }
  let rheaders = NC.responseHeaders resp
      etag = getETagHeader rheaders
  maybe
    (throwError $ MErrValidation MErrVETagHeaderNotFound)
    (return . PartInfo partNumber) etag

-- | Complete a multipart upload.
completeMultipartUpload :: Bucket -> Object -> UploadId -> [PartInfo]
                        -> Minio ETag
completeMultipartUpload bucket object uploadId partInfo = do
  resp <- executeRequest $
          def { riMethod = HT.methodPost
              , riBucket = Just bucket
              , riObject = Just object
              , riQueryParams = [("uploadId", Just $ encodeUtf8 uploadId)]
              , riPayload = PayloadBS $
                            mkCompleteMultipartUploadRequest partInfo
              }
  parseCompleteMultipartUploadResponse $ NC.responseBody resp

-- | Abort a multipart upload.
abortMultipartUpload :: Bucket -> Object -> UploadId -> Minio ()
abortMultipartUpload bucket object uploadId = do
  void $ executeRequest $ def { riMethod = HT.methodDelete
                              , riBucket = Just bucket
                              , riObject = Just object
                              , riQueryParams = [("uploadId",
                                                  Just $ encodeUtf8 uploadId)]
                              }
