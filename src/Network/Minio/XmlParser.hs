module Network.Minio.XmlParser
  ( parseListBuckets
  , parseLocation
  , parseNewMultipartUpload
  , parseCompleteMultipartUploadResponse
  , parseListObjectsResponse
  ) where

import Text.XML
import Text.XML.Cursor
import qualified Data.Text as T
import Data.Time
import Data.Text.Read (decimal)
import Data.List (zip4)

import           Lib.Prelude

import Network.Minio.Data

-- | Represent the time format string returned by S3 API calls.
s3TimeFormat :: [Char]
s3TimeFormat = iso8601DateFormat $ Just "%T%QZ"

-- | Parse the response XML of a list buckets call.
parseListBuckets :: (MonadError MinioErr m) => LByteString -> m [BucketInfo]
parseListBuckets xmldata = do
  doc <- either (throwError . MErrXml . show) return $ parseLBS def xmldata
  let cursor = fromDocument doc
      names = cursor $// element (s3Name "Bucket") &//
              element (s3Name "Name") &/ content
      timeStrings = cursor $// element (s3Name "Bucket") &//
                    element (s3Name "CreationDate") &/ content
  times <- either (throwError . MErrXml) return $
           mapM (parseTimeM True defaultTimeLocale s3TimeFormat . T.unpack)
           timeStrings
  return $ map (\(n, t) -> BucketInfo n t) $ zip names times

-- | Parse the response XML of a location request.
parseLocation :: (MonadError MinioErr m) => LByteString -> m Region
parseLocation xmldata = do
  doc <- either (throwError . MErrXml . show) return $ parseLBS def xmldata
  return $ T.concat $ fromDocument doc $/ content

-- | Parse the response XML of an newMultipartUpload call.
parseNewMultipartUpload :: (MonadError MinioErr m)
                        => LByteString -> m UploadId
parseNewMultipartUpload xmldata = do
  doc <- either (throwError . MErrXml . show) return $ parseLBS def xmldata
  return $ T.concat $ fromDocument doc
    $// element (s3Name "UploadId") &/ content

-- | Parse the response XML of completeMultipartUpload call.
parseCompleteMultipartUploadResponse :: (MonadError MinioErr m)
                                     => LByteString -> m ETag
parseCompleteMultipartUploadResponse xmldata = do
  doc <- either (throwError . MErrXml . show) return $ parseLBS def xmldata
  return $ T.concat $ fromDocument doc
    $// element (s3Name "ETag") &/ content

parseListObjectsResponse :: (MonadError MinioErr m)
                         => LByteString -> m ListObjectsResult
parseListObjectsResponse xmldata = do
  doc <- either (throwError . MErrXml . show) return $ parseLBS def xmldata

  let
    root = fromDocument doc
    s3Elem = element . s3Name

    hasMore = ["true"] == (root $/ s3Elem "IsTruncated" &/ content)

    nextToken = headMay $ root $/ s3Elem "NextContinuationToken" &/ content

    prefixes = root $/ s3Elem "CommonPrefixes" &/ s3Elem "Prefix" &/ content

    keys = root $/ s3Elem "Contents" &/ s3Elem "Key" &/ content
    modTimeStr = root $/ s3Elem "Contents" &/ s3Elem "LastModified" &/ content
    etags = root $/ s3Elem "Contents" &/ s3Elem "ETag" &/ content
    sizeStr = root $/ s3Elem "Contents" &/ s3Elem "Size" &/ content

  modTimes <- either (throwError . MErrXml) return $
    mapM (parseTimeM True defaultTimeLocale s3TimeFormat . T.unpack) $
    modTimeStr

  sizes <- forM sizeStr $ \str ->
    either (throwError . MErrXml . show) return $ fst <$> decimal str

  let
    uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
    uncurry4 f (a, b, c, d) = f a b c d

    objects = map (uncurry4 ObjectInfo) $ zip4 keys modTimes etags sizes

  return $ ListObjectsResult hasMore nextToken objects prefixes
