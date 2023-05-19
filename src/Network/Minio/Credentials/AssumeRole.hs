--
-- MinIO Haskell SDK, (C) 2017-2023 MinIO, Inc.
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

module Network.Minio.Credentials.AssumeRole where

import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Time.Units (Second)
import Lib.Prelude (UTCTime, throwIO)
import Network.HTTP.Client (RequestBody (RequestBodyBS))
import qualified Network.HTTP.Client as NC
import Network.HTTP.Types (hContentType, methodPost, renderSimpleQuery)
import Network.HTTP.Types.Header (hHost)
import Network.Minio.Credentials.Types
import Network.Minio.Data.Crypto (hashSHA256)
import Network.Minio.Errors (MErrV (..))
import Network.Minio.Sign.V4
import Network.Minio.Utils (getHostHeader, httpLbs)
import Network.Minio.XmlCommon
import Text.XML.Cursor hiding (bool)

stsVersion :: ByteString
stsVersion = "2011-06-15"

defaultDurationSeconds :: Second
defaultDurationSeconds = 3600

-- | Assume Role API argument.
--
-- @since 1.7.0
data STSAssumeRole = STSAssumeRole
  { -- | Credentials to use in the AssumeRole STS API.
    sarCredentials :: CredentialValue,
    -- | Optional settings.
    sarOptions :: STSAssumeRoleOptions
  }

-- | Options for STS Assume Role API.
data STSAssumeRoleOptions = STSAssumeRoleOptions
  { -- | STS endpoint to which the request will be made. For MinIO, this is the
    -- same as the server endpoint. For AWS, this has to be the Security Token
    -- Service endpoint. If using with 'setSTSCredential', this option can be
    -- left as 'Nothing' and the endpoint in 'ConnectInfo' will be used.
    saroEndpoint :: Maybe Text,
    -- | Desired validity for the generated credentials.
    saroDurationSeconds :: Maybe Second,
    -- | IAM policy to apply for the generated credentials.
    saroPolicyJSON :: Maybe ByteString,
    -- | Location is usually required for AWS.
    saroLocation :: Maybe Text,
    saroRoleARN :: Maybe Text,
    saroRoleSessionName :: Maybe Text
  }

-- | Default STS Assume Role options - all options are Nothing, except for
-- duration which is set to 1 hour.
defaultSTSAssumeRoleOptions :: STSAssumeRoleOptions
defaultSTSAssumeRoleOptions =
  STSAssumeRoleOptions
    { saroEndpoint = Nothing,
      saroDurationSeconds = Just 3600,
      saroPolicyJSON = Nothing,
      saroLocation = Nothing,
      saroRoleARN = Nothing,
      saroRoleSessionName = Nothing
    }

data AssumeRoleCredentials = AssumeRoleCredentials
  { arcCredentials :: CredentialValue,
    arcExpiration :: UTCTime
  }
  deriving stock (Show, Eq)

data AssumeRoleResult = AssumeRoleResult
  { arrSourceIdentity :: Text,
    arrAssumedRoleArn :: Text,
    arrAssumedRoleId :: Text,
    arrRoleCredentials :: AssumeRoleCredentials
  }
  deriving stock (Show, Eq)

-- | parseSTSAssumeRoleResult parses an XML response of the following form:
--
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
parseSTSAssumeRoleResult :: (MonadIO m) => ByteString -> Text -> m AssumeRoleResult
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

instance STSCredentialProvider STSAssumeRole where
  getSTSEndpoint = saroEndpoint . sarOptions
  retrieveSTSCredentials sar (host', port', isSecure') mgr = do
    -- Assemble STS request
    let requiredParams =
          [ ("Action", "AssumeRole"),
            ("Version", stsVersion)
          ]
        opts = sarOptions sar

        durSecs :: Int =
          fromIntegral $
            fromMaybe defaultDurationSeconds $
              saroDurationSeconds opts
        otherParams =
          [ ("RoleArn",) . encodeUtf8 <$> saroRoleARN opts,
            ("RoleSessionName",) . encodeUtf8 <$> saroRoleSessionName opts,
            Just ("DurationSeconds", show durSecs),
            ("Policy",) <$> saroPolicyJSON opts
          ]
        parameters = requiredParams ++ catMaybes otherParams
        (host, port, isSecure) =
          case getSTSEndpoint sar of
            Just ep ->
              let endPt = NC.parseRequest_ $ toString ep
               in (NC.host endPt, NC.port endPt, NC.secure endPt)
            Nothing -> (host', port', isSecure')
        reqBody = renderSimpleQuery False parameters
        req =
          NC.defaultRequest
            { NC.host = host,
              NC.port = port,
              NC.secure = isSecure,
              NC.method = methodPost,
              NC.requestHeaders =
                [ (hHost, getHostHeader (host, port)),
                  (hContentType, "application/x-www-form-urlencoded")
                ],
              NC.requestBody = RequestBodyBS reqBody
            }

    -- Sign the STS request.
    timeStamp <- liftIO Time.getCurrentTime
    let sp =
          SignParams
            { spAccessKey = coerce $ cvAccessKey $ sarCredentials sar,
              spSecretKey = coerce $ cvSecretKey $ sarCredentials sar,
              spSessionToken = coerce $ cvSessionToken $ sarCredentials sar,
              spService = ServiceSTS,
              spTimeStamp = timeStamp,
              spRegion = saroLocation opts,
              spExpirySecs = Nothing,
              spPayloadHash = Just $ hashSHA256 reqBody
            }
        signHeaders = signV4 sp req
        signedReq =
          req
            { NC.requestHeaders = NC.requestHeaders req ++ signHeaders
            }

    -- Make the STS request
    resp <- httpLbs signedReq mgr
    result <-
      parseSTSAssumeRoleResult
        (toStrict $ NC.responseBody resp)
        "https://sts.amazonaws.com/doc/2011-06-15/"
    return
      ( arcCredentials $ arrRoleCredentials result,
        coerce $ arcExpiration $ arrRoleCredentials result
      )
