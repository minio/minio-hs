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

module Network.Minio.XmlParser
  ( parseListBuckets
  , parseLocation
  , parseNewMultipartUpload
  , parseCompleteMultipartUploadResponse
  , parseCopyObjectResponse
  , parseListObjectsResponse
  , parseListObjectsV1Response
  , parseListUploadsResponse
  , parseListPartsResponse
  , parseErrResponse
  , parseNotification
  ) where

import           Control.Monad.Trans.Resource
import           Data.List                    (zip3, zip4, zip5)
import qualified Data.Text                    as T
import qualified Data.Map                     as Map
import           Data.Text.Read               (decimal)
import           Data.Time
import           Text.XML
import           Text.XML.Cursor              hiding (bool)

import           Lib.Prelude

import           Network.Minio.Data
import           Network.Minio.Errors


-- | Represent the time format string returned by S3 API calls.
s3TimeFormat :: [Char]
s3TimeFormat = iso8601DateFormat $ Just "%T%QZ"

-- | Helper functions.
uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f (a, b, c, d, e) = f a b c d e

-- | Parse time strings from XML
parseS3XMLTime :: (MonadThrow m) => Text -> m UTCTime
parseS3XMLTime = either (throwM . MErrVXmlParse) return
               . parseTimeM True defaultTimeLocale s3TimeFormat
               . T.unpack

parseDecimal :: (MonadThrow m, Integral a) => Text -> m a
parseDecimal numStr = either (throwM . MErrVXmlParse . show) return $
                      fst <$> decimal numStr

parseDecimals :: (MonadThrow m, Integral a) => [Text] -> m [a]
parseDecimals numStr = forM numStr parseDecimal

s3Elem :: Text -> Axis
s3Elem = element . s3Name

parseRoot :: (MonadThrow m) => LByteString -> m Cursor
parseRoot = either (throwM . MErrVXmlParse . show) (return . fromDocument)
          . parseLBS def

-- | Parse the response XML of a list buckets call.
parseListBuckets :: (MonadThrow m) => LByteString -> m [BucketInfo]
parseListBuckets xmldata = do
  r <- parseRoot xmldata
  let
    names = r $// s3Elem "Bucket" &// s3Elem "Name" &/ content
    timeStrings = r $// s3Elem "Bucket" &// s3Elem "CreationDate" &/ content

  times <- mapM parseS3XMLTime timeStrings
  return $ zipWith BucketInfo names times

-- | Parse the response XML of a location request.
parseLocation :: (MonadThrow m) => LByteString -> m Region
parseLocation xmldata = do
  r <- parseRoot xmldata
  let region = T.concat $ r $/ content
  return $ bool "us-east-1" region $ region /= ""

-- | Parse the response XML of an newMultipartUpload call.
parseNewMultipartUpload :: (MonadThrow m) => LByteString -> m UploadId
parseNewMultipartUpload xmldata = do
  r <- parseRoot xmldata
  return $ T.concat $ r $// s3Elem "UploadId" &/ content

-- | Parse the response XML of completeMultipartUpload call.
parseCompleteMultipartUploadResponse :: (MonadThrow m) => LByteString -> m ETag
parseCompleteMultipartUploadResponse xmldata = do
  r <- parseRoot xmldata
  return $ T.concat $ r $// s3Elem "ETag" &/ content

-- | Parse the response XML of copyObject and copyObjectPart
parseCopyObjectResponse :: (MonadThrow m) => LByteString -> m (ETag, UTCTime)
parseCopyObjectResponse xmldata = do
  r <- parseRoot xmldata
  let
    mtimeStr = T.concat $ r $// s3Elem "LastModified" &/ content

  mtime <- parseS3XMLTime mtimeStr
  return (T.concat $ r $// s3Elem "ETag" &/ content, mtime)

-- | Parse the response XML of a list objects v1 call.
parseListObjectsV1Response :: (MonadThrow m)
                         => LByteString -> m ListObjectsV1Result
parseListObjectsV1Response xmldata = do
  r <- parseRoot xmldata
  let
    hasMore = ["true"] == (r $/ s3Elem "IsTruncated" &/ content)

    nextMarker = headMay $ r $/ s3Elem "NextMarker" &/ content

    prefixes = r $/ s3Elem "CommonPrefixes" &/ s3Elem "Prefix" &/ content

    keys = r $/ s3Elem "Contents" &/ s3Elem "Key" &/ content
    modTimeStr = r $/ s3Elem "Contents" &/ s3Elem "LastModified" &/ content
    etagsList = r $/ s3Elem "Contents" &/ s3Elem "ETag" &/ content
    -- if response xml contains empty etag response fill them with as
    -- many empty Text for the zip4 below to work as intended.
    etags = etagsList ++ repeat ""
    sizeStr = r $/ s3Elem "Contents" &/ s3Elem "Size" &/ content

  modTimes <- mapM parseS3XMLTime modTimeStr
  sizes <- parseDecimals sizeStr

  let
    objects = map (uncurry5 ObjectInfo) $ zip5 keys modTimes etags sizes (repeat Map.empty)

  return $ ListObjectsV1Result hasMore nextMarker objects prefixes

-- | Parse the response XML of a list objects call.
parseListObjectsResponse :: (MonadThrow m) => LByteString -> m ListObjectsResult
parseListObjectsResponse xmldata = do
  r <- parseRoot xmldata
  let
    hasMore = ["true"] == (r $/ s3Elem "IsTruncated" &/ content)

    nextToken = headMay $ r $/ s3Elem "NextContinuationToken" &/ content

    prefixes = r $/ s3Elem "CommonPrefixes" &/ s3Elem "Prefix" &/ content

    keys = r $/ s3Elem "Contents" &/ s3Elem "Key" &/ content
    modTimeStr = r $/ s3Elem "Contents" &/ s3Elem "LastModified" &/ content
    etagsList = r $/ s3Elem "Contents" &/ s3Elem "ETag" &/ content
    -- if response xml contains empty etag response fill them with as
    -- many empty Text for the zip4 below to work as intended.
    etags = etagsList ++ repeat ""
    sizeStr = r $/ s3Elem "Contents" &/ s3Elem "Size" &/ content

  modTimes <- mapM parseS3XMLTime modTimeStr
  sizes <- parseDecimals sizeStr

  let
    objects = map (uncurry5 ObjectInfo) $ zip5 keys modTimes etags sizes (repeat Map.empty)

  return $ ListObjectsResult hasMore nextToken objects prefixes

-- | Parse the response XML of a list incomplete multipart upload call.
parseListUploadsResponse :: (MonadThrow m) => LByteString -> m ListUploadsResult
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
    uploads = zip3 uploadKeys uploadIds uploadInitTimes

  return $ ListUploadsResult hasMore nextKey nextUpload uploads prefixes

parseListPartsResponse :: (MonadThrow m) => LByteString -> m ListPartsResult
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
    partInfos = map (uncurry4 ObjectPartInfo) $
                zip4 partNumbers partETags partSizes partModTimes

  return $ ListPartsResult hasMore (listToMaybe nextPartNum) partInfos


parseErrResponse :: (MonadThrow m) => LByteString -> m ServiceErr
parseErrResponse xmldata = do
  r <- parseRoot xmldata
  let code = T.concat $ r $/ element "Code" &/ content
      message = T.concat $ r $/ element "Message" &/ content
  return $ toServiceErr code message

parseNotification :: (MonadThrow m) => LByteString -> m Notification
parseNotification xmldata = do
  r <- parseRoot xmldata
  let qcfg = map node $ r $/ s3Elem "QueueConfiguration"
      tcfg = map node $ r $/ s3Elem "TopicConfiguration"
      lcfg = map node $ r $/ s3Elem "CloudFunctionConfiguration"
  Notification <$> (mapM (parseNode "Queue") qcfg)
    <*> (mapM (parseNode "Topic") tcfg)
    <*> (mapM (parseNode "CloudFunction") lcfg)
  where

    getFilterRule c =
      let name = T.concat $ c $/ s3Elem "Name" &/ content
          value = T.concat $ c $/ s3Elem "Value" &/ content
      in FilterRule name value

    parseNode arnName nodeData = do
      let c = fromNode nodeData
          id = T.concat $ c $/ s3Elem "Id" &/ content
          arn = T.concat $ c $/ s3Elem arnName &/ content
          events = catMaybes $ map textToEvent $ c $/ s3Elem "Event" &/ content
          rules = c $/ s3Elem "Filter" &/ s3Elem "S3Key" &/
                  s3Elem "FilterRule" &| getFilterRule
      return $ NotificationConfig id arn events
        (Filter $ FilterKey $ FilterRules rules)
