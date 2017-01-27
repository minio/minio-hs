module Network.Minio
  (

    ConnectInfo(..)
  , connect

  , Minio
  , runMinio

  -- * Error handling
  -----------------------
  -- | Data types representing various errors that may occur while working
  -- with an object storage service.
  , MinioErr(..)
  , MErrV(..)

  -- * Data Types
  ----------------
  -- | Data types representing various object store concepts.
  , Bucket
  , Object
  , BucketInfo(..)
  , UploadId

  -- * Bucket and Object Operations
  ---------------------------------
  , getService
  , getLocation

  , fGetObject
  , fPutObject
  , ObjectData(..)
  , putObject
  ) where

{-
This module exports the high-level Minio API for object storage.
-}

-- import qualified Control.Monad.Trans.Resource as R
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB

import           Lib.Prelude

import           Network.Minio.Data
import           Network.Minio.PutObject
import           Network.Minio.S3API
-- import           Network.Minio.Utils

-- | Fetch the object and write it to the given file safely. The
-- object is first written to a temporary file in the same directory
-- and then moved to the given path.
fGetObject :: Bucket -> Object -> FilePath -> Minio ()
fGetObject bucket object fp = do
  (_, src) <- getObject bucket object [] []
  src C.$$+- CB.sinkFileCautious fp

-- | Upload the given file to the given object.
fPutObject :: Bucket -> Object -> FilePath -> Minio ()
fPutObject bucket object f = void $ putObject bucket object $
                             ODFile f Nothing
