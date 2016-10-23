module Network.Minio.Sign.V4
  (
    signV4
  , signV4AtTime
  , getScope
  , getHeadersToSign
  , getCanonicalRequest
  , SignV4Data(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
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

data SignV4Data = SignV4Data {
    sv4SignTime :: UTCTime
  , sv4Scope :: ByteString
  , sv4CanonicalRequest :: ByteString
  , sv4HeadersToSign :: [(ByteString, ByteString)]
  , sv4InputHeaders :: [Header]
  , sv4OutputHeaders :: [Header]
  , sv4StringToSign :: ByteString
  , sv4SigningKey :: ByteString
  } deriving (Show)

debugPrintSignV4Data :: SignV4Data -> IO ()
debugPrintSignV4Data (SignV4Data t s cr h2s ih oh sts sk) = do
  B8.putStrLn "SignV4Data:"
  B8.putStr "Timestamp: " >> print t
  B8.putStr "Scope: " >> B8.putStrLn s
  B8.putStrLn "Canonical Request:"
  B8.putStrLn cr
  B8.putStr "Headers to Sign: " >> print h2s
  B8.putStr "Input headers: " >> print ih
  B8.putStr "Output headers: " >> print oh
  B8.putStr "StringToSign: " >> B8.putStrLn sts
  B8.putStr "SigningKey: " >> printBytes sk
  B8.putStrLn "END of SignV4Data ========="
  where
    printBytes b = do
      mapM_ (\x -> B.putStr $ B.concat [show x,  " "]) $ B.unpack b
      B8.putStrLn ""

-- | Given MinioClient and request details, including request method,
-- request path, headers, query params and payload hash, generates an
-- updated set of headers, including the x-amz-date header and the
-- Authorization header, which includes the signature.
signV4 :: MinioClient -> RequestInfo
       -> IO [Header]
signV4 mc ri = do
  timestamp <- Time.getCurrentTime
  let signData = signV4AtTime timestamp mc ri
  debugPrintSignV4Data signData
  return $ sv4OutputHeaders signData

-- | Takes a timestamp, server params and request params and generates
-- an updated list of headers.
signV4AtTime :: UTCTime -> MinioClient -> RequestInfo -> SignV4Data
signV4AtTime ts mc ri =
  SignV4Data ts scope canonicalRequest headersToSign (headers ri) outHeaders stringToSign signingKey
  where
    outHeaders = authHeader : headersWithDate
    timeBS = awsTimeFormatBS ts
    dateHeader = (mk "X-Amz-Date", timeBS)
    hostHeader = (mk "host", encodeUtf8 $ mcEndPointHost mc)

    headersWithDate = dateHeader : hostHeader : (headers ri)

    authHeader = (mk "Authorization", authHeaderValue)

    scope = getScope ts mc

    authHeaderValue = B.concat [
      "AWS4-HMAC-SHA256 Credential=",
      encodeUtf8 (mcAccessKey mc), "/", scope,
      ", SignedHeaders=", signedHeaders,
      ", Signature=", signature
      ]

    headersToSign = getHeadersToSign headersWithDate

    signedHeaders = B.intercalate ";" $ map fst headersToSign

    signature = digestToBase16 $ hmacSHA256 stringToSign signingKey

    signingKey = hmacSHA256RawBS "aws4_request"
               . hmacSHA256RawBS "s3"
               . hmacSHA256RawBS (encodeUtf8 $ mcRegion mc)
               . hmacSHA256RawBS (awsDateFormatBS ts)
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
  payloadHash ri
  ]
  where
    path = getPathFromRI ri

    canonicalQueryString = B.intercalate "&" $
      map (\(x, y) -> B.concat [x, "=", y]) $
      sort $ map (\(x, y) ->
                    (uriEncode True x, maybe "" (uriEncode True) y)) $
      queryParams ri

    canonicalHeaders = B.concat $
      map (\(x, y) -> B.concat [x, ":", y, "\n"]) $
      headersForSign

    signedHeaders = B.intercalate ";" $ map fst headersForSign
