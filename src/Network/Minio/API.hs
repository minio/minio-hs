module Network.Minio.API
  (
    minioExecute
  ) where

import           Network.HTTP.Client (defaultManagerSettings)
import qualified Network.HTTP.Types as HT
import           Network.HTTP.Conduit (Response)
import qualified Network.HTTP.Conduit as NC

import           Lib.Prelude

import           Network.Minio.Data
import           Network.Minio.Data.Crypto
import           Network.Minio.Sign.V4

minioExecute :: MinioClient -> RequestInfo -> IO (Response LByteString)
minioExecute mc ri = do
  mgr <- NC.newManager defaultManagerSettings
  finalHeaders <- signV4 mc updatedRI
  NC.httpLbs (req finalHeaders) mgr
  where
    req h = NC.defaultRequest {
        NC.method         = method ri
      , NC.secure         = mcIsSecure mc
      , NC.host           = encodeUtf8 $ mcEndPointHost mc
      , NC.port           = mcEndPointPort mc
      , NC.path           = getPathFromRI ri
      , NC.queryString    = HT.renderQuery False $ queryParams ri
      , NC.requestHeaders = h
      , NC.requestBody    = NC.RequestBodyBS (payload ri)
      }

    phash = hashSHA256 $ payload ri
    updatedRI = ri {
        payloadHash = phash
      , headers = ("x-amz-content-sha256", phash) : (headers ri)
      }
