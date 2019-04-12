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

module Network.Minio.Sign.V4
  (
    signV4
  , signV4PostPolicy
  , mkScope
  , getHeadersToSign
  , mkCanonicalRequest
  , mkStringToSign
  , mkSigningKey
  , computeSignature
  , SignV4Data(..)
  , SignParams(..)
  , debugPrintSignV4Data
  ) where

import qualified Data.ByteString               as B
import qualified Data.ByteString.Base64        as Base64
import qualified Data.ByteString.Char8         as B8
import           Data.CaseInsensitive          (mk)
import qualified Data.CaseInsensitive          as CI
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Time                     as Time
import qualified Network.HTTP.Conduit          as NC
import           Network.HTTP.Types            (Header, parseQuery)
import qualified Network.HTTP.Types.Header     as H

import           Lib.Prelude

import           Network.Minio.Data.ByteString
import           Network.Minio.Data.Crypto
import           Network.Minio.Data.Time

-- these headers are not included in the string to sign when signing a
-- request
ignoredHeaders :: Set ByteString
ignoredHeaders = Set.fromList $ map CI.foldedCase
                 [ H.hAuthorization
                 , H.hContentType
                 , H.hContentLength
                 , H.hUserAgent
                 ]

data SignV4Data = SignV4Data {
      sv4SignTime         :: UTCTime
    , sv4Scope            :: ByteString
    , sv4CanonicalRequest :: ByteString
    , sv4HeadersToSign    :: [(ByteString, ByteString)]
    , sv4Output           :: [(ByteString, ByteString)]
    , sv4StringToSign     :: ByteString
    , sv4SigningKey       :: ByteString
    } deriving (Show)

data SignParams = SignParams {
      spAccessKey   :: Text
    , spSecretKey   :: Text
    , spTimeStamp   :: UTCTime
    , spRegion      :: Maybe Text
    , spExpirySecs  :: Maybe Int
    , spPayloadHash :: Maybe ByteString
    } deriving (Show)

debugPrintSignV4Data :: SignV4Data -> IO ()
debugPrintSignV4Data (SignV4Data t s cr h2s o sts sk) = do
  B8.putStrLn "SignV4Data:"
  B8.putStr "Timestamp: " >> print t
  B8.putStr "Scope: " >> B8.putStrLn s
  B8.putStrLn "Canonical Request:"
  B8.putStrLn cr
  B8.putStr "Headers to Sign: " >> print h2s
  B8.putStr "Output: " >> print o
  B8.putStr "StringToSign: " >> B8.putStrLn sts
  B8.putStr "SigningKey: " >> printBytes sk
  B8.putStrLn "END of SignV4Data ========="
  where
    printBytes b = do
      mapM_ (\x -> B.putStr $ B.concat [show x,  " "]) $ B.unpack b
      B8.putStrLn ""

-- | Given SignParams and request details, including request method,
-- request path, headers, query params and payload hash, generates an
-- updated set of headers, including the x-amz-date header and the
-- Authorization header, which includes the signature.
--
-- For normal requests (i.e. without an expiry time), the output is
-- the list of headers to add to authenticate the request.
--
-- If `expiry` is not Nothing, it is assumed that a presigned request
-- is being created. The expiry is interpreted as an integer number of
-- seconds. The output will be the list of query-parameters to add to
-- the request.

signV4 :: SignParams -> NC.Request -> [(ByteString, ByteString)]
signV4 !sp !req =
  let
    region = fromMaybe "" $ spRegion sp
    ts = spTimeStamp sp
    scope = mkScope ts region
    accessKey = toS $ spAccessKey sp
    secretKey = toS $ spSecretKey sp
    expiry = spExpirySecs sp

    -- headers to be added to the request
    datePair = ("X-Amz-Date", awsTimeFormatBS ts)
    computedHeaders = NC.requestHeaders req ++
                      if isJust $ expiry
                      then []
                      else [(\(x, y) -> (mk x, y)) datePair]
    headersToSign = getHeadersToSign computedHeaders
    signedHeaderKeys = B.intercalate ";" $ sort $ map fst headersToSign

    -- query-parameters to be added before signing for presigned URLs
    -- (i.e. when `isJust expiry`)
    authQP = [ ("X-Amz-Algorithm", "AWS4-HMAC-SHA256")
             , ("X-Amz-Credential", B.concat [accessKey, "/", scope])
             , datePair
             , ("X-Amz-Expires", maybe "" show expiry)
             , ("X-Amz-SignedHeaders", signedHeaderKeys)
             ]
    finalQP = parseQuery (NC.queryString req)  ++
              if isJust expiry
              then (fmap . fmap) Just authQP
              else []

    -- 1. compute canonical request
    canonicalRequest = mkCanonicalRequest sp (NC.setQueryString finalQP req)
                       headersToSign

    -- 2. compute string to sign
    stringToSign = mkStringToSign ts scope canonicalRequest

    -- 3.1 compute signing key
    signingKey = mkSigningKey ts region secretKey

    -- 3.2 compute signature
    signature = computeSignature stringToSign signingKey

    -- 4. compute auth header
    authValue = B.concat
      [ "AWS4-HMAC-SHA256 Credential="
      , accessKey
      , "/"
      , scope
      , ", SignedHeaders="
      , signedHeaderKeys
      , ", Signature="
      , signature
      ]
    authHeader = (H.hAuthorization, authValue)

    -- finally compute output pairs
    output = if isJust expiry
             then ("X-Amz-Signature", signature) : authQP
             else [(\(x, y) -> (CI.foldedCase x, y)) authHeader,
                   datePair]

  in output


mkScope :: UTCTime -> Text -> ByteString
mkScope ts region = B.intercalate "/"
  [ toS $ Time.formatTime Time.defaultTimeLocale "%Y%m%d" ts
  , toS region
  , "s3"
  , "aws4_request"
  ]

getHeadersToSign :: [Header] -> [(ByteString, ByteString)]
getHeadersToSign !h =
  filter (flip Set.notMember ignoredHeaders . fst) $
  map (\(x, y) -> (CI.foldedCase x, stripBS y)) h

mkCanonicalRequest :: SignParams -> NC.Request -> [(ByteString, ByteString)]
                    -> ByteString
mkCanonicalRequest !sp !req !headersForSign =
  let
    canonicalQueryString = B.intercalate "&" $
      map (\(x, y) -> B.concat [x, "=", y]) $
      sort $ map (\(x, y) ->
                    (uriEncode True x, maybe "" (uriEncode True) y)) $
      (parseQuery $ NC.queryString req)

    sortedHeaders = sort headersForSign

    canonicalHeaders = B.concat $
      map (\(x, y) -> B.concat [x, ":", y, "\n"]) sortedHeaders

    signedHeaders = B.intercalate ";" $ map fst sortedHeaders

  in
    B.intercalate "\n"
    [ NC.method req
    , uriEncode False $ NC.path req
    , canonicalQueryString
    , canonicalHeaders
    , signedHeaders
    , maybe "UNSIGNED-PAYLOAD" identity $ spPayloadHash sp
    ]

mkStringToSign :: UTCTime -> ByteString -> ByteString -> ByteString
mkStringToSign ts !scope !canonicalRequest = B.intercalate "\n"
                                             [ "AWS4-HMAC-SHA256"
                                             , awsTimeFormatBS ts
                                             , scope
                                             , hashSHA256 canonicalRequest
                                             ]

mkSigningKey :: UTCTime -> Text -> ByteString -> ByteString
mkSigningKey ts region !secretKey = hmacSHA256RawBS "aws4_request"
                                  . hmacSHA256RawBS "s3"
                                  . hmacSHA256RawBS (toS region)
                                  . hmacSHA256RawBS (awsDateFormatBS ts)
                                  $ B.concat ["AWS4", secretKey]

computeSignature :: ByteString -> ByteString -> ByteString
computeSignature !toSign !key = digestToBase16 $ hmacSHA256 toSign key

-- | Takes a validated Post Policy JSON bytestring, the signing time,
-- and ConnInfo and returns form-data for the POST upload containing
-- just the signature and the encoded post-policy.
signV4PostPolicy :: ByteString -> SignParams
                 -> Map.Map Text ByteString
signV4PostPolicy !postPolicyJSON !sp =
  let
    stringToSign = Base64.encode postPolicyJSON
    region = fromMaybe "" $ spRegion sp
    signingKey = mkSigningKey (spTimeStamp sp) region $ toS $ spSecretKey sp
    signature = computeSignature stringToSign signingKey
  in
    Map.fromList [ ("x-amz-signature", signature)
                 , ("policy", stringToSign)
                 ]
