--
-- Minio Haskell SDK, (C) 2017, 2018 Minio, Inc.
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



module Network.Minio
  (

  -- * Connecting to object storage
  ---------------------------------
    ConnectInfo(..)
  , awsCI
  , gcsCI

  -- ** Connection helpers
  ------------------------
  , awsWithRegionCI
  , minioPlayCI
  , minioCI

  -- * Minio Monad
  ----------------
  -- | The Minio monad provides connection-reuse, bucket-location
  -- caching, resource management and simpler error handling
  -- functionality. All actions on object storage are performed within
  -- this Monad.

  , Minio
  , runMinio
  , def

  -- * Bucket Operations
  ----------------------

  -- ** Creation, removal and querying
  , Bucket
  , makeBucket
  , removeBucket
  , bucketExists
  , Region
  , getLocation

  -- ** Listing
  , BucketInfo(..)
  , listBuckets

  -- ** Object info type represents object metadata information.
  , ObjectInfo
  , oiObject
  , oiModTime
  , oiETag
  , oiSize
  , oiMetadata

  , listObjects
  , listObjectsV1

  , UploadId
  , UploadInfo(..)
  , listIncompleteUploads
  , ObjectPartInfo(..)
  , listIncompleteParts

  -- ** Bucket Notifications
  , Notification(..)
  , NotificationConfig(..)
  , Arn
  , Event(..)
  , Filter(..)
  , FilterKey(..)
  , FilterRules(..)
  , FilterRule(..)
  , getBucketNotification
  , putBucketNotification
  , removeAllBucketNotification

  -- * Object Operations
  ----------------------
  , Object

  -- ** File operations
  , fGetObject
  , fPutObject

  -- ** Conduit-based streaming operations
  , putObject
  -- | Input data type represents PutObject options.
  , PutObjectOptions
  , pooContentType
  , pooContentEncoding
  , pooContentDisposition
  , pooContentLanguage
  , pooCacheControl
  , pooStorageClass
  , pooUserMetadata
  , pooNumThreads

  , getObject
  -- | Input data type represents GetObject options.
  , GetObjectOptions
  , gooRange
  , gooIfMatch
  , gooIfNoneMatch
  , gooIfModifiedSince
  , gooIfUnmodifiedSince

  -- ** Server-side copying
  , copyObject
  , SourceInfo
  , srcBucket
  , srcObject
  , srcRange
  , srcIfMatch
  , srcIfNoneMatch
  , srcIfModifiedSince
  , srcIfUnmodifiedSince
  , DestinationInfo
  , dstBucket
  , dstObject

  -- ** Querying
  , statObject

  -- ** Object removal functions
  , removeObject
  , removeIncompleteUpload

  -- * Presigned Operations
  -------------------------
  , UrlExpiry
  , presignedPutObjectUrl
  , presignedGetObjectUrl
  , presignedHeadObjectUrl

  -- ** Utilities for POST (browser) uploads
  , PostPolicy
  , PostPolicyError(..)
  , newPostPolicy
  , presignedPostPolicy
  , showPostPolicy

  -- *** Utilities to specify Post Policy conditions
  , PostPolicyCondition
  , ppCondBucket
  , ppCondContentLengthRange
  , ppCondContentType
  , ppCondKey
  , ppCondKeyStartsWith
  , ppCondSuccessActionStatus

  -- * Error handling
  -----------------------
  -- | Data types representing various errors that may occur while working
  -- with an object storage service.
  , MinioErr(..)
  , MErrV(..)
  , ServiceErr(..)

) where

{-
This module exports the high-level Minio API for object storage.
-}

import qualified Data.Conduit             as C
import qualified Data.Conduit.Binary      as CB
import qualified Data.Conduit.Combinators as CC
import           Data.Default             (def)

import           Lib.Prelude

import           Network.Minio.CopyObject
import           Network.Minio.Data
import           Network.Minio.Errors
import           Network.Minio.ListOps
import           Network.Minio.PutObject
import           Network.Minio.S3API
import           Network.Minio.Utils

-- | Lists buckets.
listBuckets :: Minio [BucketInfo]
listBuckets = getService

-- | Fetch the object and write it to the given file safely. The
-- object is first written to a temporary file in the same directory
-- and then moved to the given path.
fGetObject :: Bucket -> Object -> FilePath -> GetObjectOptions -> Minio ()
fGetObject bucket object fp opts = do
  src <- getObject bucket object opts
  C.connect src $ CB.sinkFileCautious fp

-- | Upload the given file to the given object.
fPutObject :: Bucket -> Object -> FilePath
           -> PutObjectOptions -> Minio ()
fPutObject bucket object f opts =
  void $ putObjectInternal bucket object opts $ ODFile f Nothing

-- | Put an object from a conduit source. The size can be provided if
-- known; this helps the library select optimal part sizes to perform
-- a multipart upload. If not specified, it is assumed that the object
-- can be potentially 5TiB and selects multipart sizes appropriately.
putObject :: Bucket -> Object -> C.ConduitM () ByteString Minio ()
          -> Maybe Int64 -> PutObjectOptions -> Minio ()
putObject bucket object src sizeMay opts =
  void $ putObjectInternal bucket object opts $ ODStream src sizeMay

-- | Perform a server-side copy operation to create an object based on
-- the destination specification in DestinationInfo from the source
-- specification in SourceInfo. This function performs a multipart
-- copy operation if the new object is to be greater than 5GiB in
-- size.
copyObject :: DestinationInfo -> SourceInfo -> Minio ()
copyObject dstInfo srcInfo = void $ copyObjectInternal (dstBucket dstInfo)
                             (dstObject dstInfo) srcInfo

-- | Remove an object from the object store.
removeObject :: Bucket -> Object -> Minio ()
removeObject = deleteObject

-- | Get an object from the object store as a resumable source (conduit).
getObject :: Bucket -> Object -> GetObjectOptions
          -> Minio (C.ConduitM () ByteString Minio ())
getObject bucket object opts = snd <$> getObject' bucket object []
                               (gooToHeaders opts)

-- | Get an object's metadata from the object store.
statObject :: Bucket -> Object -> Minio ObjectInfo
statObject = headObject

-- | Creates a new bucket in the object store. The Region can be
-- optionally specified. If not specified, it will use the region
-- configured in ConnectInfo, which is by default, the US Standard
-- Region.
makeBucket :: Bucket -> Maybe Region -> Minio ()
makeBucket bucket regionMay = do
  region <- maybe (asks $ connectRegion . mcConnInfo) return regionMay
  putBucket bucket region
  addToRegionCache bucket region

-- | Removes a bucket from the object store.
removeBucket :: Bucket -> Minio ()
removeBucket bucket = do
  deleteBucket bucket
  deleteFromRegionCache bucket

-- | Query the object store if a given bucket is present.
bucketExists :: Bucket -> Minio Bool
bucketExists = headBucket

-- | Removes an ongoing multipart upload of an object.
removeIncompleteUpload :: Bucket -> Object -> Minio ()
removeIncompleteUpload bucket object = do
  uploads <- C.runConduit $ listIncompleteUploads bucket (Just object) False
             C..| CC.sinkList
  mapM_ (abortMultipartUpload bucket object) (uiUploadId <$> uploads)
