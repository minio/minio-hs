module Network.Minio.ListOps where

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import           Lib.Prelude

import           Network.Minio.Data
import           Network.Minio.S3API

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
listIncompleteUploads :: Bucket -> Maybe Text -> Bool
                      -> C.Producer Minio UploadInfo
listIncompleteUploads bucket prefix recurse = loop Nothing Nothing
  where
    loop :: Maybe Text -> Maybe Text -> C.Producer Minio UploadInfo
    loop nextKeyMarker nextUploadIdMarker = do
      let
        delimiter = bool (Just "/") Nothing recurse

      res <- lift $ listIncompleteUploads' bucket prefix delimiter
             nextKeyMarker nextUploadIdMarker
      CL.sourceList $ lurUploads res
      when (lurHasMore res) $
        loop nextKeyMarker nextUploadIdMarker

-- | List object parts of an ongoing multipart upload for given
-- bucket, object and uploadId.
listIncompleteParts :: Bucket -> Object -> UploadId
                    -> C.Producer Minio ListPartInfo
listIncompleteParts bucket object uploadId = loop Nothing
  where
    loop :: Maybe Text -> C.Producer Minio ListPartInfo
    loop nextPartMarker = do
      res <- lift $ listIncompleteParts' bucket object uploadId Nothing
             nextPartMarker
      CL.sourceList $ lprParts res
      when (lprHasMore res) $
        loop (show <$> lprNextPart res)
