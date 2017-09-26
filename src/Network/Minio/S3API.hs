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

module Network.Minio.S3API
  (
    Region
  , getLocation

  -- * Listing buckets
  --------------------
  , getService

  -- * Listing objects
  --------------------
  , ListObjectsResult(..)
  , listObjects'

  -- * Retrieving buckets
  , headBucket
  -- * Retrieving objects
  -----------------------
  , getObject'
  , headObject

  -- * Creating buckets and objects
  ---------------------------------
  , putBucket
  , ETag
  , putObjectSingle
  , copyObjectSingle

  -- * Multipart Upload APIs
  --------------------------
  , UploadId
  , PartTuple
  , Payload(..)
  , PartNumber
  , CopyPartSource(..)
  , newMultipartUpload
  , putObjectPart
  , copyObjectPart
  , completeMultipartUpload
  , abortMultipartUpload
  , ListUploadsResult(..)
  , listIncompleteUploads'
  , ListPartsResult(..)
  , listIncompleteParts'

  -- * Deletion APIs
  --------------------------
  , deleteBucket
  , deleteObject

  -- * Presigned Operations
  -----------------------------
  , module Network.Minio.PresignedOperations
  ) where

import           Control.Monad.Catch (catches, Handler(..))
import qualified Data.Conduit as C
import           Data.Default (def)
import qualified Network.HTTP.Conduit as NC
import qualified Network.HTTP.Types as HT
import           Network.HTTP.Types.Status (status404)

import           Lib.Prelude hiding (catches)

import           Network.Minio.API
import           Network.Minio.Data
import           Network.Minio.Errors
import           Network.Minio.Utils
import           Network.Minio.XmlGenerator
import           Network.Minio.XmlParser
import           Network.Minio.PresignedOperations


-- | Fetch all buckets from the service.
getService :: Minio [BucketInfo]
getService = do
  resp <- executeRequest $ def {
      riNeedsLocation = False
    }
  parseListBuckets $ NC.responseBody resp

-- | GET an object from the service and return the response headers
-- and a conduit source for the object content
getObject' :: Bucket -> Object -> HT.Query -> [HT.Header]
           -> Minio ([HT.Header], C.ResumableSource Minio ByteString)
getObject' bucket object queryParams headers = do
  resp <- mkStreamRequest reqInfo
  return (NC.responseHeaders resp, NC.responseBody resp)
  where
    reqInfo = def { riBucket = Just bucket
                  , riObject = Just object
                  , riQueryParams = queryParams
                  , riHeaders = headers
                  }

-- | Creates a bucket via a PUT bucket call.
putBucket :: Bucket -> Region -> Minio ()
putBucket bucket location = void $
  executeRequest $
    def { riMethod = HT.methodPut
        , riBucket = Just bucket
        , riPayload = PayloadBS $ mkCreateBucketConfig location
        , riNeedsLocation = False
        }

-- | Single PUT object size.
maxSinglePutObjectSizeBytes :: Int64
maxSinglePutObjectSizeBytes = 5 * 1024 * 1024 * 1024

-- | PUT an object into the service. This function performs a single
-- PUT object call, and so can only transfer objects upto 5GiB.
putObjectSingle :: Bucket -> Object -> [HT.Header] -> Handle -> Int64
                -> Int64 -> Minio ETag
putObjectSingle bucket object headers h offset size = do
  -- check length is within single PUT object size.
  when (size > maxSinglePutObjectSizeBytes) $
    throwM $ MErrVSinglePUTSizeExceeded size

  -- content-length header is automatically set by library.
  resp <- executeRequest $
          def { riMethod = HT.methodPut
              , riBucket = Just bucket
              , riObject = Just object
              , riHeaders = headers
              , riPayload = PayloadH h offset size
              }

  let rheaders = NC.responseHeaders resp
      etag = getETagHeader rheaders
  maybe
    (throwM MErrVETagHeaderNotFound)
    return etag

-- | List objects in a bucket matching prefix up to delimiter,
-- starting from nextToken.
listObjects' :: Bucket -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int
            -> Minio ListObjectsResult
listObjects' bucket prefix nextToken delimiter maxKeys = do
  resp <- executeRequest $ def { riMethod = HT.methodGet
                               , riBucket = Just bucket
                               , riQueryParams = mkOptionalParams params
                               }
  parseListObjectsResponse $ NC.responseBody resp
  where
    params = [
        ("list-type", Just "2")
      , ("continuation_token", nextToken)
      , ("prefix", prefix)
      , ("delimiter", delimiter)
      , ("max-keys", show <$> maxKeys)
      ]

-- | DELETE a bucket from the service.
deleteBucket :: Bucket -> Minio ()
deleteBucket bucket = void $
  executeRequest $
    def { riMethod = HT.methodDelete
        , riBucket = Just bucket
        }

-- | DELETE an object from the service.
deleteObject :: Bucket -> Object -> Minio ()
deleteObject bucket object = void $
  executeRequest $
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

-- | PUT a part of an object as part of a multipart upload.
putObjectPart :: Bucket -> Object -> UploadId -> PartNumber -> [HT.Header]
              -> Payload -> Minio PartTuple
putObjectPart bucket object uploadId partNumber headers payload = do
  resp <- executeRequest $
          def { riMethod = HT.methodPut
              , riBucket = Just bucket
              , riObject = Just object
              , riQueryParams = mkOptionalParams params
              , riHeaders = headers
              , riPayload = payload
              }
  let rheaders = NC.responseHeaders resp
      etag = getETagHeader rheaders
  maybe
    (throwM MErrVETagHeaderNotFound)
    (return . (partNumber, )) etag
  where
    params = [
        ("uploadId", Just uploadId)
      , ("partNumber", Just $ show partNumber)
      ]

-- | Performs server-side copy of an object or part of an object as an
-- upload part of an ongoing multi-part upload.
copyObjectPart :: Bucket -> Object -> CopyPartSource -> UploadId
               -> PartNumber -> [HT.Header] -> Minio (ETag, UTCTime)
copyObjectPart bucket object cps uploadId partNumber headers = do
  resp <- executeRequest $
          def { riMethod = HT.methodPut
              , riBucket = Just bucket
              , riObject = Just object
              , riQueryParams = mkOptionalParams params
              , riHeaders = headers ++ cpsToHeaders cps
              }

  parseCopyObjectResponse $ NC.responseBody resp
  where
    params = [
        ("uploadId", Just uploadId)
      , ("partNumber", Just $ show partNumber)
      ]

-- | Performs server-side copy of an object that is upto 5GiB in
-- size. If the object is greater than 5GiB, this function throws the
-- error returned by the server.
copyObjectSingle :: Bucket -> Object -> CopyPartSource -> [HT.Header]
                 -> Minio (ETag, UTCTime)
copyObjectSingle bucket object cps headers = do
  -- validate that cpSourceRange is Nothing for this API.
  when (isJust $ cpSourceRange cps) $
    throwM MErrVCopyObjSingleNoRangeAccepted
  resp <- executeRequest $
          def { riMethod = HT.methodPut
              , riBucket = Just bucket
              , riObject = Just object
              , riHeaders = headers ++ cpsToHeaders cps
              }
  parseCopyObjectResponse $ NC.responseBody resp

-- | Complete a multipart upload.
completeMultipartUpload :: Bucket -> Object -> UploadId -> [PartTuple]
                        -> Minio ETag
completeMultipartUpload bucket object uploadId partTuple = do
  resp <- executeRequest $
          def { riMethod = HT.methodPost
              , riBucket = Just bucket
              , riObject = Just object
              , riQueryParams = mkOptionalParams params
              , riPayload = PayloadBS $
                            mkCompleteMultipartUploadRequest partTuple
              }
  parseCompleteMultipartUploadResponse $ NC.responseBody resp
  where
    params = [("uploadId", Just uploadId)]

-- | Abort a multipart upload.
abortMultipartUpload :: Bucket -> Object -> UploadId -> Minio ()
abortMultipartUpload bucket object uploadId = void $
  executeRequest $ def { riMethod = HT.methodDelete
                              , riBucket = Just bucket
                              , riObject = Just object
                              , riQueryParams = mkOptionalParams params
                              }
  where
    params = [("uploadId", Just uploadId)]

-- | List incomplete multipart uploads.
listIncompleteUploads' :: Bucket -> Maybe Text -> Maybe Text -> Maybe Text
                       -> Maybe Text -> Maybe Int -> Minio ListUploadsResult
listIncompleteUploads' bucket prefix delimiter keyMarker uploadIdMarker maxKeys = do
  resp <- executeRequest $ def { riMethod = HT.methodGet
                               , riBucket = Just bucket
                               , riQueryParams = params
                               }
  parseListUploadsResponse $ NC.responseBody resp
  where
    -- build query params
    params = ("uploads", Nothing) : mkOptionalParams
             [ ("prefix", prefix)
             , ("delimiter", delimiter)
             , ("key-marker", keyMarker)
             , ("upload-id-marker", uploadIdMarker)
             , ("max-uploads", show <$> maxKeys)
             ]


-- | List parts of an ongoing multipart upload.
listIncompleteParts' :: Bucket -> Object -> UploadId -> Maybe Text
                     -> Maybe Text -> Minio ListPartsResult
listIncompleteParts' bucket object uploadId maxParts partNumMarker = do
  resp <- executeRequest $ def { riMethod = HT.methodGet
                               , riBucket = Just bucket
                               , riObject = Just object
                               , riQueryParams = mkOptionalParams params
                               }
  parseListPartsResponse $ NC.responseBody resp
  where
    -- build optional query params
    params = [
        ("uploadId", Just uploadId)
      , ("part-number-marker", partNumMarker)
      , ("max-parts", maxParts)
      ]

-- | Get metadata of an object.
headObject :: Bucket -> Object -> Minio ObjectInfo
headObject bucket object = do
  resp <- executeRequest $ def { riMethod = HT.methodHead
                               , riBucket = Just bucket
                               , riObject = Just object
                               }

  let
    headers = NC.responseHeaders resp
    modTime = getLastModifiedHeader headers
    etag = getETagHeader headers
    size = getContentLength headers

  maybe (throwM MErrVInvalidObjectInfoResponse) return $
    ObjectInfo <$> Just object <*> modTime <*> etag <*> size



-- | Query the object store if a given bucket exists.
headBucket :: Bucket -> Minio Bool
headBucket bucket = headBucketEx `catches`
                    [ Handler handleNoSuchBucket
                    , Handler handleStatus404
                    ]

  where
    handleNoSuchBucket :: ServiceErr -> Minio Bool
    handleNoSuchBucket e | e == NoSuchBucket = return False
                         | otherwise = throwM e

    handleStatus404 :: NC.HttpException -> Minio Bool
    handleStatus404 e@(NC.HttpExceptionRequest _ (NC.StatusCodeException res _)) =
      if NC.responseStatus res == status404
      then return False
      else throwM e
    handleStatus404 e = throwM e

    headBucketEx = do
      resp <- executeRequest $ def { riMethod = HT.methodHead
                                   , riBucket = Just bucket
                                   }
      return $ NC.responseStatus resp == HT.ok200
