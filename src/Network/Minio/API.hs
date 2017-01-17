module Network.Minio.API
  (
    connect
  , defaultConnectInfo
  , RequestInfo(..)
  , runMinio
  , executeRequest
  , mkStreamRequest
  , requestInfo
  ) where

import qualified Network.HTTP.Types as HT
import           Network.HTTP.Conduit (Response)
import qualified Network.HTTP.Conduit as NC
import           Network.HTTP.Types (Method, Header, Query)
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

payloadBodyWithHash :: (MonadIO m) => RequestInfo
                    -> m (ByteString, NC.RequestBody)
payloadBodyWithHash ri = case riPayload ri of
  EPayload -> return (hashSHA256 "", NC.RequestBodyBS "")
  PayloadBS bs -> return (hashSHA256 bs, NC.RequestBodyBS bs)
  PayloadH h off size -> do
    let offM = return . fromIntegral $ off
        sizeM = return . fromIntegral $ size
    hash <- hashSHA256FromSource $ sourceHandleRange h offM sizeM
    return (hash, NC.requestBodySource (fromIntegral size) $
                  sourceHandleRange h offM sizeM)

buildRequest :: (MonadIO m, MonadReader MinioConn m)
             => RequestInfo -> m NC.Request
buildRequest ri = do
  (phash, rbody) <- payloadBodyWithHash ri
  let newRi = ri {
          riPayloadHash = phash
        , riHeaders = ("x-amz-content-sha256", phash) : (riHeaders ri)
        }

  ci <- asks mcConnInfo

  reqHeaders <- liftIO $ signV4 ci newRi

  return NC.defaultRequest {
      NC.method = riMethod newRi
    , NC.secure = connectIsSecure ci
    , NC.host = encodeUtf8 $ connectHost ci
    , NC.port = connectPort ci
    , NC.path = getPathFromRI ri
    , NC.queryString = HT.renderQuery False $ riQueryParams ri
    , NC.requestHeaders = reqHeaders
    , NC.requestBody = rbody
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


requestInfo :: Method -> Maybe Bucket -> Maybe Object
            -> Query -> [Header] -> Payload
            -> RequestInfo
requestInfo m b o q h p = RequestInfo m b o q h p "" Nothing
