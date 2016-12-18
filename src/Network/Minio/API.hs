module Network.Minio.API
  (
    connect
  , defaultConnectInfo
  , RequestInfo(..)
  , runMinio
  , getService
  ) where

import qualified Network.HTTP.Types as HT
import           Network.HTTP.Conduit (Response)
import qualified Network.HTTP.Conduit as NC
import           Network.HTTP.Types (Method, Header, Query)

import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)

import           Lib.Prelude

import qualified Data.Conduit as C

import           Network.Minio.Data
import           Network.Minio.Data.Crypto
import           Network.Minio.Sign.V4

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

mkSRequest :: RequestInfo -> Minio (Response (C.ResumableSource Minio ByteString))
mkSRequest ri = do
  let PayloadSingle pload = payload ri
      phash = hashSHA256 pload
      newRI = ri {
          payloadHash = phash
        , headers = ("x-amz-content-sha256", phash) : (headers ri)
        }

  ci <- asks mcConnInfo

  reqHeaders <- liftIO $ signV4 ci newRI

  mgr <- asks mcConnManager

  let req = NC.defaultRequest {
          NC.method = method newRI
        , NC.secure = connectIsSecure ci
        , NC.host = encodeUtf8 $ connectHost ci
        , NC.port = connectPort ci
        , NC.path = getPathFromRI ri
        , NC.queryString = HT.renderQuery False $ queryParams ri
        , NC.requestHeaders = reqHeaders
        , NC.requestBody = NC.RequestBodyBS pload
        }

  response <- NC.http req mgr
  return response

requestInfo :: Method -> Maybe Bucket -> Maybe Object
            -> Query -> [Header] -> Payload
            -> RequestInfo
requestInfo m b o q h p = RequestInfo m b o q h p ""

-- getService :: Minio _ -- ResponseInfo
getService = mkSRequest $
  requestInfo HT.methodGet Nothing Nothing [] [] $
  PayloadSingle ""
