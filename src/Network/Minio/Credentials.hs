--
-- MinIO Haskell SDK, (C) 2017-2022 MinIO, Inc.
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

module Network.Minio.Credentials
  ( CredentialValue (..),
    CredentialProvider (..),
    AccessKey,
    SecretKey,
    SessionToken,
    defaultSTSAssumeRoleOptions,
    STSAssumeRole (..),
    STSAssumeRoleOptions (..),
  )
where

import qualified Data.Time as Time
import Data.Time.Units (Second)
import Network.HTTP.Client (RequestBody (RequestBodyBS))
import qualified Network.HTTP.Client as NC
import qualified Network.HTTP.Client.TLS as NC
import Network.HTTP.Types (hContentType, methodPost, renderSimpleQuery)
import Network.HTTP.Types.Header (hHost)
import Network.Minio.Data
import Network.Minio.Data.Crypto (hashSHA256)
import Network.Minio.Sign.V4
import Network.Minio.Utils (httpLbs)
import Network.Minio.XmlParser (parseSTSAssumeRoleResult)

class CredentialProvider p where
  retrieveCredentials :: p -> IO CredentialValue

stsVersion :: ByteString
stsVersion = "2011-06-15"

defaultDurationSeconds :: Second
defaultDurationSeconds = 3600

data STSAssumeRole = STSAssumeRole
  { sarEndpoint :: Text,
    sarCredentials :: CredentialValue,
    sarOptions :: STSAssumeRoleOptions
  }

data STSAssumeRoleOptions = STSAssumeRoleOptions
  { -- | Desired validity for the generated credentials.
    saroDurationSeconds :: Maybe Second,
    -- | IAM policy to apply for the generated credentials.
    saroPolicyJSON :: Maybe ByteString,
    -- | Location is usually required for AWS.
    saroLocation :: Maybe Text,
    saroRoleARN :: Maybe Text,
    saroRoleSessionName :: Maybe Text,
    -- | Optional HTTP connection manager
    saroHTTPManager :: Maybe NC.Manager
  }

-- | Default STS Assume Role options
defaultSTSAssumeRoleOptions :: STSAssumeRoleOptions
defaultSTSAssumeRoleOptions =
  STSAssumeRoleOptions
    { saroDurationSeconds = Just defaultDurationSeconds,
      saroPolicyJSON = Nothing,
      saroLocation = Nothing,
      saroRoleARN = Nothing,
      saroRoleSessionName = Nothing,
      saroHTTPManager = Nothing
    }

instance CredentialProvider STSAssumeRole where
  retrieveCredentials sar = do
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
          let endPt = NC.parseRequest_ $ toString $ sarEndpoint sar
           in (NC.host endPt, NC.port endPt, NC.secure endPt)
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
        settings = bool NC.defaultManagerSettings NC.tlsManagerSettings isSecure

    -- Make the STS request
    mgr <- maybe (NC.newManager settings) return $ saroHTTPManager opts
    resp <- httpLbs signedReq mgr
    result <-
      parseSTSAssumeRoleResult
        (toStrict $ NC.responseBody resp)
        "https://sts.amazonaws.com/doc/2011-06-15/"
    return $ arcCredentials $ arrRoleCredentials result
