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
  (
    connect
  , RequestInfo(..)
  , runMinio
  , executeRequest
  , mkStreamRequest
  , getLocation
  ) where

import qualified Data.Conduit as C
import           Data.Conduit.Binary (sourceHandleRange)
import           Data.Default (def)
import qualified Data.Map as Map
import           Network.HTTP.Conduit (Response)
import qualified Network.HTTP.Conduit as NC
import qualified Network.HTTP.Types as HT

import           Lib.Prelude

import           Network.Minio.Data
import           Network.Minio.Data.Crypto
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
  regionMay <- gets (Map.lookup bucket)
  maybe (do
            l <- lift $ getLocation bucket
            modify $ Map.insert bucket l
            return l
        ) return regionMay


buildRequest :: RequestInfo -> Minio NC.Request
buildRequest ri = do
  {-
    If ListBuckets/MakeBucket/GetLocation then use connectRegion ci
    Else If discovery off use connectRegion ci
    Else {

    // Here discovery is on
    Lookup region in regionMap
    If present use that
    Else getLocation
    }
  -}
  ci <- asks mcConnInfo
  region <- if | not $ riNeedsLocation ri -> -- getService/makeBucket/getLocation
                                             -- don't need location
                   return $ Just $ connectRegion ci
               | not $ connectAutoDiscoverRegion ci -> -- if autodiscovery of location is disabled by user
                   return $ Just $ connectRegion ci
               | otherwise -> discoverRegion ri

  sha256Hash <- getPayloadSHA256Hash (riPayload ri)
  let newRi = ri {
          riPayloadHash = sha256Hash
        , riHeaders = sha256Header sha256Hash : (riHeaders ri)
        , riRegion = region
        }

  reqHeaders <- liftIO $ signV4 ci newRi

  return NC.defaultRequest {
      NC.method = riMethod newRi
    , NC.secure = connectIsSecure ci
    , NC.host = encodeUtf8 $ connectHost ci
    , NC.port = connectPort ci
    , NC.path = getPathFromRI newRi
    , NC.queryString = HT.renderQuery False $ riQueryParams newRi
    , NC.requestHeaders = reqHeaders
    , NC.requestBody = getRequestBody (riPayload newRi)
    }

executeRequest :: RequestInfo -> Minio (Response LByteString)
executeRequest ri = do
  req <- buildRequest ri
  mgr <- asks mcConnManager
  httpLbs req mgr


mkStreamRequest :: RequestInfo
                -> Minio (Response (C.ResumableSource Minio ByteString))
mkStreamRequest ri = do
  req <- buildRequest ri
  mgr <- asks mcConnManager
  http req mgr
