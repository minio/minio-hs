module Network.Minio.XmlParser
  ( parseListBuckets
  , parseLocation
  , parseNewMultipartUpload
  , parseCompleteMultipartUploadResponse
  , parseListObjectsResponse
  ) where

import           Data.List (zip4)
import qualified Data.Text as T
import           Data.Text.Read (decimal)
import           Data.Time
import           Text.XML
import           Text.XML.Cursor

import           Lib.Prelude

import           Network.Minio.Data

-- | Represent the time format string returned by S3 API calls.
s3TimeFormat :: [Char]
s3TimeFormat = iso8601DateFormat $ Just "%T%QZ"

-- | Parse time strings from XML
parseS3XMLTime :: (MonadError MinioErr m) => Text -> m UTCTime
parseS3XMLTime = either (throwError . MErrXml) return
               . parseTimeM True defaultTimeLocale s3TimeFormat
               . T.unpack

s3Elem :: Text -> Axis
s3Elem = element . s3Name

parseRoot :: (MonadError MinioErr m) => LByteString -> m Cursor
parseRoot = either (throwError . MErrXml . show) (return . fromDocument)
          . parseLBS def

-- | Parse the response XML of a list buckets call.
parseListBuckets :: (MonadError MinioErr m) => LByteString -> m [BucketInfo]
parseListBuckets xmldata = do
  r <- parseRoot xmldata
  let
    names = r $// s3Elem "Bucket" &// s3Elem "Name" &/ content
    timeStrings = r $// s3Elem "Bucket" &// s3Elem "CreationDate" &/ content

  times <- mapM parseS3XMLTime timeStrings
  return $ map (\(n, t) -> BucketInfo n t) $ zip names times

-- | Parse the response XML of a location request.
parseLocation :: (MonadError MinioErr m) => LByteString -> m Region
parseLocation xmldata = do
  r <- parseRoot xmldata
  return $ T.concat $ r $/ content

-- | Parse the response XML of an newMultipartUpload call.
parseNewMultipartUpload :: (MonadError MinioErr m)
                        => LByteString -> m UploadId
parseNewMultipartUpload xmldata = do
  r <- parseRoot xmldata
  return $ T.concat $ r $// element (s3Name "UploadId") &/ content

-- | Parse the response XML of completeMultipartUpload call.
parseCompleteMultipartUploadResponse :: (MonadError MinioErr m)
                                     => LByteString -> m ETag
parseCompleteMultipartUploadResponse xmldata = do
  r <- parseRoot xmldata
  return $ T.concat $ r $// s3Elem "ETag" &/ content

parseListObjectsResponse :: (MonadError MinioErr m)
                         => LByteString -> m ListObjectsResult
parseListObjectsResponse xmldata = do
  r <- parseRoot xmldata
  let
    hasMore = ["true"] == (r $/ s3Elem "IsTruncated" &/ content)

    nextToken = headMay $ r $/ s3Elem "NextContinuationToken" &/ content

    prefixes = r $/ s3Elem "CommonPrefixes" &/ s3Elem "Prefix" &/ content

    keys = r $/ s3Elem "Contents" &/ s3Elem "Key" &/ content
    modTimeStr = r $/ s3Elem "Contents" &/ s3Elem "LastModified" &/ content
    etags = r $/ s3Elem "Contents" &/ s3Elem "ETag" &/ content
    sizeStr = r $/ s3Elem "Contents" &/ s3Elem "Size" &/ content

  modTimes <- mapM parseS3XMLTime modTimeStr

  sizes <- forM sizeStr $ \str ->
    either (throwError . MErrXml . show) return $ fst <$> decimal str

  let
    uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
    uncurry4 f (a, b, c, d) = f a b c d

    objects = map (uncurry4 ObjectInfo) $ zip4 keys modTimes etags sizes

  return $ ListObjectsResult hasMore nextToken objects prefixes
