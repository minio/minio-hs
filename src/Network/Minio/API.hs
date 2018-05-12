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

module Network.Minio.API
  ( connect
  , RequestInfo(..)
  , runMinio
  , executeRequest
  , mkStreamRequest
  , getLocation

  , isValidBucketName
  , checkBucketNameValidity
  , isValidObjectName
  , checkObjectNameValidity
  ) where

import qualified Data.ByteString           as B
import qualified Data.Char                 as C
import qualified Data.Conduit              as C
import           Data.Conduit.Binary       (sourceHandleRange)
import           Data.Default              (def)
import qualified Data.Map                  as Map
import qualified Data.Text                 as T

import           Network.HTTP.Conduit      (Response)
import qualified Network.HTTP.Conduit      as NC
import qualified Network.HTTP.Types        as HT
import           Network.HTTP.Types.Header (hHost)

import           Lib.Prelude

import           Network.Minio.Data
import           Network.Minio.Data.Crypto
import           Network.Minio.Errors
import           Network.Minio.Sign.V4
import           Network.Minio.Utils
import           Network.Minio.XmlParser

sha256Header :: ByteString -> HT.Header
sha256Header = ("x-amz-content-sha256", )

getPayloadSHA256Hash :: (MonadIO m) => Payload -> m ByteString
getPayloadSHA256Hash (PayloadBS bs) = return $ hashSHA256 bs
getPayloadSHA256Hash (PayloadH h off size) = hashSHA256FromSource $
  sourceHandleRange h
    (return . fromIntegral $ off)
    (return . fromIntegral $ size)

getRequestBody :: Payload -> NC.RequestBody
getRequestBody (PayloadBS bs) = NC.RequestBodyBS bs
getRequestBody (PayloadH h off size) =
  NC.requestBodySource (fromIntegral size) $
    sourceHandleRange h
      (return . fromIntegral $ off)
      (return . fromIntegral $ size)


-- | Fetch bucket location (region)
getLocation :: Bucket -> Minio Region
getLocation bucket = do
  resp <- executeRequest $ def {
      riBucket = Just bucket
    , riQueryParams = [("location", Nothing)]
    , riNeedsLocation = False
    }
  parseLocation $ NC.responseBody resp


-- | Looks for region in RegionMap and updates it using getLocation if
-- absent.
discoverRegion :: RequestInfo -> Minio (Maybe Region)
discoverRegion ri = runMaybeT $ do
  bucket <- MaybeT $ return $ riBucket ri
  regionMay <- lift $ lookupRegionCache bucket
  maybe (do
            l <- lift $ getLocation bucket
            lift $ addToRegionCache bucket l
            return l
        ) return regionMay


buildRequest :: RequestInfo -> Minio NC.Request
buildRequest ri = do
  maybe (return ()) checkBucketNameValidity $ riBucket ri
  maybe (return ()) checkObjectNameValidity $ riObject ri

  ci <- asks mcConnInfo

               -- getService/makeBucket/getLocation -- don't need
               -- location
  region <- if | not $ riNeedsLocation ri ->
                   return $ Just $ connectRegion ci

               -- if autodiscovery of location is disabled by user
               | not $ connectAutoDiscoverRegion ci ->
                   return $ Just $ connectRegion ci

               -- discover the region for the request
               | otherwise -> discoverRegion ri

  regionHost <- case region of
    Nothing ->  return $ connectHost ci
    Just r -> if "amazonaws.com" `T.isSuffixOf` connectHost ci
              then maybe
                   (throwM $ MErrVRegionNotSupported r)
                   return
                   (Map.lookup r awsRegionMap)
              else return $ connectHost ci

  sha256Hash <- if | connectIsSecure ci ->
                       -- if secure connection
                       return "UNSIGNED-PAYLOAD"

                   -- otherwise compute sha256
                   | otherwise -> getPayloadSHA256Hash (riPayload ri)

  let hostHeader = (hHost, getHostAddr ci)
      newRi = ri { riPayloadHash = Just sha256Hash
                 , riHeaders = hostHeader
                             : sha256Header sha256Hash
                             : riHeaders ri
                 , riRegion = region
                 }
      newCi = ci { connectHost = regionHost }

  signHeaders <- liftIO $ signV4 newCi newRi Nothing

  return NC.defaultRequest {
      NC.method = riMethod newRi
    , NC.secure = connectIsSecure newCi
    , NC.host = encodeUtf8 $ connectHost newCi
    , NC.port = connectPort newCi
    , NC.path = getPathFromRI newRi
    , NC.queryString = HT.renderQuery False $ riQueryParams newRi
    , NC.requestHeaders = riHeaders newRi ++ mkHeaderFromPairs signHeaders
    , NC.requestBody = getRequestBody (riPayload newRi)
    }

executeRequest :: RequestInfo -> Minio (Response LByteString)
executeRequest ri = do
  req <- buildRequest ri
  mgr <- asks mcConnManager
  httpLbs req mgr


mkStreamRequest :: RequestInfo
                -> Minio (Response (C.ConduitM () ByteString Minio ()))
mkStreamRequest ri = do
  req <- buildRequest ri
  mgr <- asks mcConnManager
  http req mgr

-- Bucket name validity check according to AWS rules.
isValidBucketName :: Bucket -> Bool
isValidBucketName bucket =
  not (or [ len < 3 || len > 63
          , or (map labelCheck labels)
          , or (map labelCharsCheck labels)
          , isIPCheck
          ])
  where
    len = T.length bucket
    labels = T.splitOn "." bucket

    -- does label `l` fail basic checks of length and start/end?
    labelCheck l = T.length l == 0 || T.head l == '-' || T.last l == '-'

    -- does label `l` have non-allowed characters?
    labelCharsCheck l = isJust $ T.find (\x -> not (C.isAsciiLower x ||
                                                    x == '-' ||
                                                    C.isDigit x)) l

    -- does label `l` have non-digit characters?
    labelNonDigits l = isJust $ T.find (not . C.isDigit) l
    labelAsNums = map (not . labelNonDigits) labels

    -- check if bucket name looks like an IP
    isIPCheck = and labelAsNums && length labelAsNums == 4

-- Throws exception iff bucket name is invalid according to AWS rules.
checkBucketNameValidity :: MonadThrow m => Bucket -> m ()
checkBucketNameValidity bucket =
  when (not $ isValidBucketName bucket) $
  throwM $ MErrVInvalidBucketName bucket

isValidObjectName :: Object -> Bool
isValidObjectName object =
  T.length object > 0 && B.length (encodeUtf8 object) <= 1024

checkObjectNameValidity :: MonadThrow m => Object -> m ()
checkObjectNameValidity object =
  when (not $ isValidObjectName object) $
  throwM $ MErrVInvalidObjectName object
