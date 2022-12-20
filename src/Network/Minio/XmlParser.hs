--
-- MinIO Haskell SDK, (C) 2017 MinIO, Inc.
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
  ( parseListBuckets,
    parseLocation,
    parseNewMultipartUpload,
    parseCompleteMultipartUploadResponse,
    parseCopyObjectResponse,
    parseListObjectsResponse,
    parseListObjectsV1Response,
    parseListUploadsResponse,
    parseListPartsResponse,
    parseErrResponse,
    parseNotification,
    parseSelectProgress,
    parseSTSAssumeRoleResult,
  )
where

import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as H
import Data.List (zip4, zip6)
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Data.Time
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Lib.Prelude
import Network.Minio.Data
import Network.Minio.Errors
import Text.XML
import Text.XML.Cursor hiding (bool)

-- | Helper functions.
uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a, b, c, d, e, f) -> g
uncurry6 f (a, b, c, d, e, g) = f a b c d e g

-- | Parse time strings from XML
parseS3XMLTime :: MonadIO m => Text -> m UTCTime
parseS3XMLTime t =
  maybe (throwIO $ MErrVXmlParse $ "timestamp parse failure: " <> t) return $
    iso8601ParseM $
      toString t

parseDecimal :: (MonadIO m, Integral a) => Text -> m a
parseDecimal numStr =
  either (throwIO . MErrVXmlParse . show) return $
    fst <$> decimal numStr

parseDecimals :: (MonadIO m, Integral a) => [Text] -> m [a]
parseDecimals numStr = forM numStr parseDecimal

s3Elem :: Text -> Text -> Axis
s3Elem ns = element . s3Name ns

parseRoot :: (MonadIO m) => LByteString -> m Cursor
parseRoot =
  either (throwIO . MErrVXmlParse . show) (return . fromDocument)
    . parseLBS def

-- | Parse the response XML of a list buckets call.
parseListBuckets :: (MonadReader env m, HasSvcNamespace env, MonadIO m) => LByteString -> m [BucketInfo]
parseListBuckets xmldata = do
  r <- parseRoot xmldata
  ns <- asks getSvcNamespace
  let s3Elem' = s3Elem ns
      names = r $// s3Elem' "Bucket" &// s3Elem' "Name" &/ content
      timeStrings = r $// s3Elem' "Bucket" &// s3Elem' "CreationDate" &/ content

  times <- mapM parseS3XMLTime timeStrings
  return $ zipWith BucketInfo names times

-- | Parse the response XML of a location request.
parseLocation :: (MonadIO m) => LByteString -> m Region
parseLocation xmldata = do
  r <- parseRoot xmldata
  let region = T.concat $ r $/ content
  return $ bool "us-east-1" region $ region /= ""

-- | Parse the response XML of an newMultipartUpload call.
parseNewMultipartUpload :: (MonadReader env m, HasSvcNamespace env, MonadIO m) => LByteString -> m UploadId
parseNewMultipartUpload xmldata = do
  r <- parseRoot xmldata
  ns <- asks getSvcNamespace
  let s3Elem' = s3Elem ns
  return $ T.concat $ r $// s3Elem' "UploadId" &/ content

-- | Parse the response XML of completeMultipartUpload call.
parseCompleteMultipartUploadResponse :: (MonadReader env m, HasSvcNamespace env, MonadIO m) => LByteString -> m ETag
parseCompleteMultipartUploadResponse xmldata = do
  r <- parseRoot xmldata
  ns <- asks getSvcNamespace
  let s3Elem' = s3Elem ns
  return $ T.concat $ r $// s3Elem' "ETag" &/ content

-- | Parse the response XML of copyObject and copyObjectPart
parseCopyObjectResponse :: (MonadReader env m, HasSvcNamespace env, MonadIO m) => LByteString -> m (ETag, UTCTime)
parseCopyObjectResponse xmldata = do
  r <- parseRoot xmldata
  ns <- asks getSvcNamespace
  let s3Elem' = s3Elem ns
      mtimeStr = T.concat $ r $// s3Elem' "LastModified" &/ content

  mtime <- parseS3XMLTime mtimeStr
  return (T.concat $ r $// s3Elem' "ETag" &/ content, mtime)

-- | Parse the response XML of a list objects v1 call.
parseListObjectsV1Response ::
  (MonadReader env m, HasSvcNamespace env, MonadIO m) =>
  LByteString ->
  m ListObjectsV1Result
parseListObjectsV1Response xmldata = do
  r <- parseRoot xmldata
  ns <- asks getSvcNamespace
  let s3Elem' = s3Elem ns
      hasMore = ["true"] == (r $/ s3Elem' "IsTruncated" &/ content)
      nextMarker = listToMaybe $ r $/ s3Elem' "NextMarker" &/ content
      prefixes = r $/ s3Elem' "CommonPrefixes" &/ s3Elem' "Prefix" &/ content
      keys = r $/ s3Elem' "Contents" &/ s3Elem' "Key" &/ content
      modTimeStr = r $/ s3Elem' "Contents" &/ s3Elem' "LastModified" &/ content
      etagsList = r $/ s3Elem' "Contents" &/ s3Elem' "ETag" &/ content
      -- if response xml contains empty etag response fill them with as
      -- many empty Text for the zip4 below to work as intended.
      etags = etagsList ++ repeat ""
      sizeStr = r $/ s3Elem' "Contents" &/ s3Elem' "Size" &/ content

  modTimes <- mapM parseS3XMLTime modTimeStr
  sizes <- parseDecimals sizeStr

  let objects =
        map (uncurry6 ObjectInfo) $
          zip6 keys modTimes etags sizes (repeat H.empty) (repeat H.empty)

  return $ ListObjectsV1Result hasMore nextMarker objects prefixes

-- | Parse the response XML of a list objects call.
parseListObjectsResponse :: (MonadReader env m, HasSvcNamespace env, MonadIO m) => LByteString -> m ListObjectsResult
parseListObjectsResponse xmldata = do
  r <- parseRoot xmldata
  ns <- asks getSvcNamespace
  let s3Elem' = s3Elem ns
      hasMore = ["true"] == (r $/ s3Elem' "IsTruncated" &/ content)
      nextToken = listToMaybe $ r $/ s3Elem' "NextContinuationToken" &/ content
      prefixes = r $/ s3Elem' "CommonPrefixes" &/ s3Elem' "Prefix" &/ content
      keys = r $/ s3Elem' "Contents" &/ s3Elem' "Key" &/ content
      modTimeStr = r $/ s3Elem' "Contents" &/ s3Elem' "LastModified" &/ content
      etagsList = r $/ s3Elem' "Contents" &/ s3Elem' "ETag" &/ content
      -- if response xml contains empty etag response fill them with as
      -- many empty Text for the zip4 below to work as intended.
      etags = etagsList ++ repeat ""
      sizeStr = r $/ s3Elem' "Contents" &/ s3Elem' "Size" &/ content

  modTimes <- mapM parseS3XMLTime modTimeStr
  sizes <- parseDecimals sizeStr

  let objects =
        map (uncurry6 ObjectInfo) $
          zip6 keys modTimes etags sizes (repeat H.empty) (repeat H.empty)

  return $ ListObjectsResult hasMore nextToken objects prefixes

-- | Parse the response XML of a list incomplete multipart upload call.
parseListUploadsResponse :: (MonadReader env m, HasSvcNamespace env, MonadIO m) => LByteString -> m ListUploadsResult
parseListUploadsResponse xmldata = do
  r <- parseRoot xmldata
  ns <- asks getSvcNamespace
  let s3Elem' = s3Elem ns
      hasMore = ["true"] == (r $/ s3Elem' "IsTruncated" &/ content)
      prefixes = r $/ s3Elem' "CommonPrefixes" &/ s3Elem' "Prefix" &/ content
      nextKey = listToMaybe $ r $/ s3Elem' "NextKeyMarker" &/ content
      nextUpload = listToMaybe $ r $/ s3Elem' "NextUploadIdMarker" &/ content
      uploadKeys = r $/ s3Elem' "Upload" &/ s3Elem' "Key" &/ content
      uploadIds = r $/ s3Elem' "Upload" &/ s3Elem' "UploadId" &/ content
      uploadInitTimeStr = r $/ s3Elem' "Upload" &/ s3Elem' "Initiated" &/ content

  uploadInitTimes <- mapM parseS3XMLTime uploadInitTimeStr

  let uploads = zip3 uploadKeys uploadIds uploadInitTimes

  return $ ListUploadsResult hasMore nextKey nextUpload uploads prefixes

parseListPartsResponse :: (MonadReader env m, HasSvcNamespace env, MonadIO m) => LByteString -> m ListPartsResult
parseListPartsResponse xmldata = do
  r <- parseRoot xmldata
  ns <- asks getSvcNamespace
  let s3Elem' = s3Elem ns
      hasMore = ["true"] == (r $/ s3Elem' "IsTruncated" &/ content)
      nextPartNumStr = listToMaybe $ r $/ s3Elem' "NextPartNumberMarker" &/ content
      partNumberStr = r $/ s3Elem' "Part" &/ s3Elem' "PartNumber" &/ content
      partModTimeStr = r $/ s3Elem' "Part" &/ s3Elem' "LastModified" &/ content
      partETags = r $/ s3Elem' "Part" &/ s3Elem' "ETag" &/ content
      partSizeStr = r $/ s3Elem' "Part" &/ s3Elem' "Size" &/ content

  partModTimes <- mapM parseS3XMLTime partModTimeStr
  partSizes <- parseDecimals partSizeStr
  partNumbers <- parseDecimals partNumberStr
  nextPartNum <- parseDecimals $ maybeToList nextPartNumStr

  let partInfos =
        map (uncurry4 ObjectPartInfo) $
          zip4 partNumbers partETags partSizes partModTimes

  return $ ListPartsResult hasMore (listToMaybe nextPartNum) partInfos

parseErrResponse :: (MonadIO m) => LByteString -> m ServiceErr
parseErrResponse xmldata = do
  r <- parseRoot xmldata
  let code = T.concat $ r $/ laxElement "Code" &/ content
      message = T.concat $ r $/ laxElement "Message" &/ content
  return $ toServiceErr code message

parseNotification :: (MonadReader env m, HasSvcNamespace env, MonadIO m) => LByteString -> m Notification
parseNotification xmldata = do
  r <- parseRoot xmldata
  ns <- asks getSvcNamespace
  let s3Elem' = s3Elem ns
      qcfg = map node $ r $/ s3Elem' "QueueConfiguration"
      tcfg = map node $ r $/ s3Elem' "TopicConfiguration"
      lcfg = map node $ r $/ s3Elem' "CloudFunctionConfiguration"
  Notification
    <$> mapM (parseNode ns "Queue") qcfg
    <*> mapM (parseNode ns "Topic") tcfg
    <*> mapM (parseNode ns "CloudFunction") lcfg
  where
    getFilterRule ns c =
      let name = T.concat $ c $/ s3Elem ns "Name" &/ content
          value = T.concat $ c $/ s3Elem ns "Value" &/ content
       in FilterRule name value
    parseNode ns arnName nodeData = do
      let c = fromNode nodeData
          itemId = T.concat $ c $/ s3Elem ns "Id" &/ content
          arn = T.concat $ c $/ s3Elem ns arnName &/ content
          events = mapMaybe textToEvent (c $/ s3Elem ns "Event" &/ content)
          rules =
            c
              $/ s3Elem ns "Filter"
              &/ s3Elem ns "S3Key"
              &/ s3Elem ns "FilterRule"
              &| getFilterRule ns
      return $
        NotificationConfig
          itemId
          arn
          events
          (Filter $ FilterKey $ FilterRules rules)

parseSelectProgress :: MonadIO m => ByteString -> m Progress
parseSelectProgress xmldata = do
  r <- parseRoot $ LB.fromStrict xmldata
  let bScanned = T.concat $ r $/ element "BytesScanned" &/ content
      bProcessed = T.concat $ r $/ element "BytesProcessed" &/ content
      bReturned = T.concat $ r $/ element "BytesReturned" &/ content
  Progress
    <$> parseDecimal bScanned
    <*> parseDecimal bProcessed
    <*> parseDecimal bReturned

-- <AssumeRoleResponse xmlns="https://sts.amazonaws.com/doc/2011-06-15/">
--   <AssumeRoleResult>
--   <SourceIdentity>Alice</SourceIdentity>
--     <AssumedRoleUser>
--       <Arn>arn:aws:sts::123456789012:assumed-role/demo/TestAR</Arn>
--       <AssumedRoleId>ARO123EXAMPLE123:TestAR</AssumedRoleId>
--     </AssumedRoleUser>
--     <Credentials>
--       <AccessKeyId>ASIAIOSFODNN7EXAMPLE</AccessKeyId>
--       <SecretAccessKey>wJalrXUtnFEMI/K7MDENG/bPxRfiCYzEXAMPLEKEY</SecretAccessKey>
--       <SessionToken>
--        AQoDYXdzEPT//////////wEXAMPLEtc764bNrC9SAPBSM22wDOk4x4HIZ8j4FZTwdQW
--        LWsKWHGBuFqwAeMicRXmxfpSPfIeoIYRqTflfKD8YUuwthAx7mSEI/qkPpKPi/kMcGd
--        QrmGdeehM4IC1NtBmUpp2wUE8phUZampKsburEDy0KPkyQDYwT7WZ0wq5VSXDvp75YU
--        9HFvlRd8Tx6q6fE8YQcHNVXAkiY9q6d+xo0rKwT38xVqr7ZD0u0iPPkUL64lIZbqBAz
--        +scqKmlzm8FDrypNC9Yjc8fPOLn9FX9KSYvKTr4rvx3iSIlTJabIQwj2ICCR/oLxBA==
--       </SessionToken>
--       <Expiration>2019-11-09T13:34:41Z</Expiration>
--     </Credentials>
--     <PackedPolicySize>6</PackedPolicySize>
--   </AssumeRoleResult>
--   <ResponseMetadata>
--     <RequestId>c6104cbe-af31-11e0-8154-cbc7ccf896c7</RequestId>
--   </ResponseMetadata>
-- </AssumeRoleResponse>

parseSTSAssumeRoleResult :: MonadIO m => ByteString -> Text -> m AssumeRoleResult
parseSTSAssumeRoleResult xmldata namespace = do
  r <- parseRoot $ LB.fromStrict xmldata
  let s3Elem' = s3Elem namespace
      sourceIdentity =
        T.concat $
          r
            $/ s3Elem' "AssumeRoleResult"
            &/ s3Elem' "SourceIdentity"
            &/ content
      roleArn =
        T.concat $
          r
            $/ s3Elem' "AssumeRoleResult"
            &/ s3Elem' "AssumedRoleUser"
            &/ s3Elem' "Arn"
            &/ content
      roleId =
        T.concat $
          r
            $/ s3Elem' "AssumeRoleResult"
            &/ s3Elem' "AssumedRoleUser"
            &/ s3Elem' "AssumedRoleId"
            &/ content

      convSB :: Text -> BA.ScrubbedBytes
      convSB = BA.convert . (encodeUtf8 :: Text -> ByteString)

      credsInfo = do
        cr <-
          maybe (Left $ MErrVXmlParse "No Credentials Element found") Right $
            listToMaybe $
              r $/ s3Elem' "AssumeRoleResult" &/ s3Elem' "Credentials"
        let cur = fromNode $ node cr
        return
          ( CredentialValue
              { cvAccessKey =
                  coerce $
                    T.concat $
                      cur $/ s3Elem' "AccessKeyId" &/ content,
                cvSecretKey =
                  coerce $
                    convSB $
                      T.concat $
                        cur
                          $/ s3Elem' "SecretAccessKey"
                          &/ content,
                cvSessionToken =
                  Just $
                    coerce $
                      convSB $
                        T.concat $
                          cur
                            $/ s3Elem' "SessionToken"
                            &/ content
              },
            T.concat $ cur $/ s3Elem' "Expiration" &/ content
          )
  creds <- either throwIO pure credsInfo
  expiry <- parseS3XMLTime $ snd creds
  let roleCredentials =
        AssumeRoleCredentials
          { arcCredentials = fst creds,
            arcExpiration = expiry
          }
  return
    AssumeRoleResult
      { arrSourceIdentity = sourceIdentity,
        arrAssumedRoleArn = roleArn,
        arrAssumedRoleId = roleId,
        arrRoleCredentials = roleCredentials
      }
