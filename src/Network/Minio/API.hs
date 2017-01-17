module Network.Minio.API
  (
    connect
  , defaultConnectInfo
  , RequestInfo(..)
  , runMinio
  , executeRequest
  , mkStreamRequest
  ) where

import qualified Network.HTTP.Types as HT
import           Network.HTTP.Conduit (Response)
import qualified Network.HTTP.Conduit as NC
import qualified Data.Conduit as C
import Data.Conduit.Binary (sourceHandleRange)

import           Lib.Prelude


import           Network.Minio.Data
import           Network.Minio.Data.Crypto
import           Network.Minio.Sign.V4
import Network.Minio.Utils

-- runRequestDebug r mgr = do
--   print $ "runRequestDebug"
--   print $ NC.method r
--   print $ NC.secure r
--   print $ NC.host r
--   print $ NC.port r
--   print $ NC.path r
--   print $ NC.queryString r
--   print $ NC.requestHeaders r
--   -- print $ NC.requestBody r
--   NC.httpLbs r mgr

-- sha256Header ::  :: HT.HeaderName
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

buildRequest :: (MonadIO m, MonadReader MinioConn m)
             => RequestInfo -> m NC.Request
buildRequest ri = do
  sha256Hash <- getPayloadSHA256Hash (riPayload ri)
  let newRi = ri {
          riPayloadHash = sha256Hash
        , riHeaders = sha256Header sha256Hash : (riHeaders ri)
        }

  ci <- asks mcConnInfo

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
