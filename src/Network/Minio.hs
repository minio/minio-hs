module Network.Minio
  (

    ConnectInfo(..)
  , awsCI
  , minioPlayCI
  , connect

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
  , UploadId
  , ObjectData(..)

  -- * Bucket Operations
  ----------------------
  , getService
  , getLocation

  , listObjects
  , listIncompleteUploads
  , listIncompleteParts

  -- * Object Operations
  ----------------------
  , fGetObject
  , fPutObject
  , putObjectFromSource

  , getObject

  ) where

{-
This module exports the high-level Minio API for object storage.
-}

import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB

import           Lib.Prelude

import           Network.Minio.Data
import           Network.Minio.ListOps
import           Network.Minio.PutObject
import           Network.Minio.S3API

-- | Fetch the object and write it to the given file safely. The
-- object is first written to a temporary file in the same directory
-- and then moved to the given path.
fGetObject :: Bucket -> Object -> FilePath -> Minio ()
fGetObject bucket object fp = do
  src <- getObject bucket object
  src C.$$+- CB.sinkFileCautious fp

-- | Upload the given file to the given object.
fPutObject :: Bucket -> Object -> FilePath -> Minio ()
fPutObject bucket object f = void $ putObject bucket object $
                             ODFile f Nothing

-- | Put an object from a conduit source. The size can be provided if
-- known; this helps the library select optimal part sizes to
-- performing a multipart upload. If not specified, it is assumed that
-- the object can be potentially 5TiB and selects multipart sizes
-- appropriately.
putObjectFromSource :: Bucket -> Object -> C.Producer Minio ByteString
                    -> Maybe Int64 -> Minio ()
putObjectFromSource bucket object src sizeMay = void $ putObject bucket object $
                                                ODStream src sizeMay

-- | Get an object from the object store as a resumable source (conduit).
getObject :: Bucket -> Object -> Minio (C.ResumableSource Minio ByteString)
getObject bucket object = snd <$> getObject' bucket object [] []
