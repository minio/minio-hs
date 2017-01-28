module Network.Minio.XmlParser
  ( parseListBuckets
  , parseLocation
  , parseNewMultipartUpload
  , parseCompleteMultipartUploadResponse
  , parseListObjectsResponse
  , parseListUploadsResponse
  , parseListPartsResponse
  ) where

import Data.List (zip3, zip4)
import qualified Data.Text as T
import           Data.Text.Read (decimal)
import           Data.Time
import           Text.XML
import           Text.XML.Cursor

import           Lib.Prelude

import           Network.Minio.Data


-- | Helper functions.
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

-- | Represent the time format string returned by S3 API calls.
s3TimeFormat :: [Char]
s3TimeFormat = iso8601DateFormat $ Just "%T%QZ"

-- | Parse time strings from XML
parseS3XMLTime :: (MonadError MinioErr m) => Text -> m UTCTime
parseS3XMLTime = either (throwError . MErrXml) return
               . parseTimeM True defaultTimeLocale s3TimeFormat
               . T.unpack

parseDecimals :: (MonadError MinioErr m, Integral a) => [Text] -> m [a]
parseDecimals numStr = forM numStr $ \str ->
  either (throwError . MErrXml . show) return $ fst <$> decimal str

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

-- | Parse the response XML of a list objects call.
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

  sizes <- parseDecimals sizeStr

  let
    objects = map (uncurry4 ObjectInfo) $ zip4 keys modTimes etags sizes

  return $ ListObjectsResult hasMore nextToken objects prefixes

-- | Parse the response XML of a list incomplete multipart upload call.
parseListUploadsResponse :: (MonadError MinioErr m)
                         => LByteString -> m ListUploadsResult
parseListUploadsResponse xmldata = do
  r <- parseRoot xmldata
  let
    hasMore = ["true"] == (r $/ s3Elem "IsTruncated" &/ content)
    prefixes = r $/ s3Elem "CommonPrefixes" &/ s3Elem "Prefix" &/ content
    nextKey = headMay $ r $/ s3Elem "NextKeyMarker" &/ content
    nextUpload = headMay $ r $/ s3Elem "NextUploadIdMarker" &/ content
    uploadKeys = r $/ s3Elem "Upload" &/ s3Elem "Key" &/ content
    uploadIds = r $/ s3Elem "Upload" &/ s3Elem "UploadId" &/ content
    uploadInitTimeStr = r $/ s3Elem "Upload" &/ s3Elem "Initiated" &/ content

  uploadInitTimes <- mapM parseS3XMLTime uploadInitTimeStr

  let
    uploads = map (uncurry3 UploadInfo) $ zip3 uploadKeys uploadIds uploadInitTimes

  return $ ListUploadsResult hasMore nextKey nextUpload uploads prefixes

parseListPartsResponse :: (MonadError MinioErr m)
                       => LByteString -> m ListPartsResult
parseListPartsResponse xmldata = do
  r <- parseRoot xmldata
  let
    hasMore = ["true"] == (r $/ s3Elem "IsTruncated" &/ content)
    nextPartNumStr = headMay $ r $/ s3Elem "NextPartNumberMarker" &/ content
    partNumberStr = r $/ s3Elem "Part" &/ s3Elem "PartNumber" &/ content
    partModTimeStr = r $/ s3Elem "Part" &/ s3Elem "LastModified" &/ content
    partETags = r $/ s3Elem "Part" &/ s3Elem "ETag" &/ content
    partSizeStr = r $/ s3Elem "Part" &/ s3Elem "Size" &/ content

  partModTimes <- mapM parseS3XMLTime partModTimeStr
  partSizes <- parseDecimals partSizeStr
  partNumbers <- parseDecimals partNumberStr
  nextPartNum <- parseDecimals $ maybeToList nextPartNumStr

  let
    partInfos = map (uncurry4 ListPartInfo) $ zip4 partNumbers partETags partSizes partModTimes
  return $ ListPartsResult hasMore (listToMaybe nextPartNum) partInfos
