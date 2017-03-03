module Network.Minio
  (

    ConnectInfo(..)
  , awsCI
  , awsWithRegion
  , minioPlayCI
  , minioSimple
  , minioSimpleTLS
  , minioWithOpts

  , Minio
  , runMinio
  , runResourceT

  -- * Error handling
  -----------------------
  -- | Data types representing various errors that may occur while working
  -- with an object storage service.
  , MinioErr(..)
  , MErrV(..)
  , MError(..)

  -- * Data Types
  ----------------
  -- | Data types representing various object store concepts.
  , Bucket
  , Object
  , BucketInfo(..)
  , ObjectInfo(..)
  , UploadInfo(..)
  , ObjectPartInfo(..)
  , UploadId
  , ObjectData(..)
  , CopyPartSource(..)

  -- * Bucket Operations
  ----------------------
  , listBuckets
  , getLocation
  , makeBucket
  , removeBucket

  , listObjects
  , listIncompleteUploads

  -- * Object Operations
  ----------------------
  , fGetObject
  , fPutObject
  , putObject
  , copyObject

  , getObject
  , statObject

  ) where

{-
This module exports the high-level Minio API for object storage.
-}

import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Map as Map

import           Lib.Prelude

import           Network.Minio.Data
import           Network.Minio.Errors
import           Network.Minio.ListOps
import           Network.Minio.PutObject
import           Network.Minio.S3API

-- | Lists buckets.
listBuckets :: Minio [BucketInfo]
listBuckets = getService

-- | Fetch the object and write it to the given file safely. The
-- object is first written to a temporary file in the same directory
-- and then moved to the given path.
fGetObject :: Bucket -> Object -> FilePath -> Minio ()
fGetObject bucket object fp = do
  src <- getObject bucket object
  src C.$$+- CB.sinkFileCautious fp

-- | Upload the given file to the given object.
fPutObject :: Bucket -> Object -> FilePath -> Minio ()
fPutObject bucket object f = void $ putObjectInternal bucket object $
                             ODFile f Nothing

-- | Put an object from a conduit source. The size can be provided if
-- known; this helps the library select optimal part sizes to perform
-- a multipart upload. If not specified, it is assumed that the object
-- can be potentially 5TiB and selects multipart sizes appropriately.
putObject :: Bucket -> Object -> C.Producer Minio ByteString
          -> Maybe Int64 -> Minio ()
putObject bucket object src sizeMay =
  void $ putObjectInternal bucket object $ ODStream src sizeMay

-- | Perform a server-side copy operation to create an object with the
-- given bucket and object name from the source specification in
-- CopyPartSource. This function performs a multipart copy operation
-- if the new object is to be greater than 5GiB in size.
copyObject :: Bucket -> Object -> CopyPartSource -> Minio ()
copyObject bucket object cps = void $ copyObjectInternal bucket object cps

-- | Get an object from the object store as a resumable source (conduit).
getObject :: Bucket -> Object -> Minio (C.ResumableSource Minio ByteString)
getObject bucket object = snd <$> getObject' bucket object [] []

-- | Creates a new bucket in the object store. The Region can be
-- optionally specified. If not specified, it will use the region
-- configured in ConnectInfo, which is by default, the US Standard
-- Region.
makeBucket :: Bucket -> Maybe Region -> Minio ()
makeBucket bucket regionMay= do
  region <- maybe (asks $ connectRegion . mcConnInfo) return regionMay
  putBucket bucket region
  modify (Map.insert bucket region)

-- | Get an object's metadata from the object store.
statObject :: Bucket -> Object -> Minio ObjectInfo
statObject bucket object = headObject bucket object

removeBucket :: Bucket -> Minio()
removeBucket bucket = do
  deleteBucket bucket
  modify (Map.delete bucket)
