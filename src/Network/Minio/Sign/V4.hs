module Network.Minio.Sign.V4
  (
    signV4
  ) where

import qualified Data.ByteString as B
import           Data.ByteString.Char8 (pack)
import           Data.CaseInsensitive (mk)
import qualified Data.CaseInsensitive as CI
import qualified Data.Set as Set
import qualified Data.Time as Time
import           Network.HTTP.Types (Header)

import           Lib.Prelude
import           Network.Minio.Data
import           Network.Minio.Data.ByteString
import           Network.Minio.Data.Crypto
import           Network.Minio.Data.Time

ignoredHeaders :: Set ByteString
ignoredHeaders = Set.fromList $ map CI.foldedCase [
  mk "Authorization",
  mk "Content-Type",
  mk "Content-Length",
  mk "User-Agent"
  ]

-- | Given MinioClient and request details, including request method,
-- request path, headers, query params and payload hash, generates an
-- updated set of headers, including the x-amz-date header and the
-- Authorization header, which includes the signature.
signV4 :: MinioClient -> RequestInfo
       -> IO [Header]
signV4 mc ri = do
  timestamp <- Time.getCurrentTime
  return $ signV4AtTime timestamp mc ri

-- | Takes a timestamp, server params and request params and generates
-- an updated list of headers.
signV4AtTime :: UTCTime -> MinioClient -> RequestInfo -> [Header]
signV4AtTime ts mc ri = authHeader : headersWithDate
  where
    timeBS = awsTimeFormatBS ts
    dateHeader = (mk "X-Amz-Date", timeBS)

    headersWithDate = dateHeader : (headers ri)

    authHeader = (mk "Authorization", authHeaderValue)

    scope = getScope ts mc

    authHeaderValue = B.concat [
      "AWS4-HMAC-SHA256 Credential=",
      scope,
      ", SignedHeaders=", signedHeaders,
      ", Signature=", signature
      ]

    headersToSign = getHeadersToSign headersWithDate

    signedHeaders = B.intercalate ";" $ map fst headersToSign

    signature = hmacSHA256 stringToSign signingKey

    signingKey = hmacSHA256 "aws4_request"
               . hmacSHA256 "s3"
               . hmacSHA256 (encodeUtf8 $ mcRegion mc)
               . hmacSHA256 timeBS
               $ (B.concat ["AWS4", encodeUtf8 $ mcSecretKey mc])

    stringToSign  = B.intercalate "\n" $
      ["AWS4-HMAC-SHA256",
       timeBS,
       scope,
       hashSHA256 $ canonicalRequest
      ]

    canonicalRequest = getCanonicalRequest ri headersToSign


getScope :: UTCTime -> MinioClient -> ByteString
getScope ts mc = B.intercalate "/" $ [
  encodeUtf8 (mcAccessKey mc),
  pack $ Time.formatTime Time.defaultTimeLocale "%Y%m%d" ts,
  "us-east-1", "s3", "aws4_request"
  ]

getHeadersToSign :: [Header] -> [(ByteString, ByteString)]
getHeadersToSign h =
  sort $
  filter (flip Set.notMember ignoredHeaders . fst) $
  map (\(x, y) -> (CI.foldedCase x, stripBS y)) $
  h

getCanonicalRequest :: RequestInfo -> [(ByteString, ByteString)] -> ByteString
getCanonicalRequest ri headersForSign = B.intercalate "\n" $ [
  method ri,
  uriEncode False path,
  canonicalQueryString,
  canonicalHeaders,
  signedHeaders,
  payloadHash ri,
  ""
  ]
  where
    path = B.concat $
      maybe [] (\bkt -> bkt : (
                   maybe [] (\obj ->
                                ["/", encodeUtf8 $ obj]) $ object ri)) $
      bucket ri

    canonicalQueryString = B.intercalate "&" $
      map (\(x, y) -> B.concat [x, "=", y]) $
      sort $ map (\(x, y) ->
                    (uriEncode True x, maybe "" (uriEncode True) y)) $
      queryParams ri

    canonicalHeaders = B.concat $
      map (\(x, y) -> B.concat [x, ":", y, "\n"]) $
      headersForSign

    signedHeaders = B.intercalate ";" $ map fst headersForSign
