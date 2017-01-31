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
  , listObjects
  , listIncompleteUploads
  , listIncompleteParts
  ) where

{-
This module exports the high-level Minio API for object storage.
-}

-- import qualified Control.Monad.Trans.Resource as R
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

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

-- | List objects in a bucket matching the given prefix. If recurse is
-- set to True objects matching prefix are recursively listed.
listObjects :: Bucket -> Maybe Text -> Bool -> C.Producer Minio ObjectInfo
listObjects bucket prefix recurse = loop Nothing
  where
    loop :: Maybe Text -> C.Producer Minio ObjectInfo
    loop nextToken = do
      let
        delimiter = bool (Just "/") Nothing recurse

      res <- lift $ listObjects' bucket prefix nextToken delimiter
      CL.sourceList $ lorObjects res
      when (lorHasMore res) $
        loop (lorNextToken res)

-- | List incomplete uploads in a bucket matching the given prefix. If
-- recurse is set to True incomplete uploads for the given prefix are
-- recursively listed.
listIncompleteUploads :: Bucket -> Maybe Text -> Bool -> C.Producer Minio UploadInfo
listIncompleteUploads bucket prefix recurse = loop Nothing Nothing
  where
    loop :: Maybe Text -> Maybe Text -> C.Producer Minio UploadInfo
    loop nextKeyMarker nextUploadIdMarker = do
      let
        delimiter = bool (Just "/") Nothing recurse

      res <- lift $ listIncompleteUploads' bucket prefix delimiter nextKeyMarker nextUploadIdMarker
      CL.sourceList $ lurUploads res
      when (lurHasMore res) $
        loop nextKeyMarker nextUploadIdMarker

-- | List object parts of an ongoing multipart upload for given
-- bucket, object and uploadId.
listIncompleteParts :: Bucket -> Object -> UploadId -> C.Producer Minio ListPartInfo
listIncompleteParts bucket object uploadId = loop Nothing
  where
    loop :: Maybe Text -> C.Producer Minio ListPartInfo
    loop nextPartMarker = do
      res <- lift $ listIncompleteParts' bucket object uploadId Nothing nextPartMarker
      CL.sourceList $ lprParts res
      when (lprHasMore res) $
        loop (show <$> lprNextPart res)
