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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Network.Minio.Credentials.Types where

import qualified Data.ByteArray as BA
import Lib.Prelude (UTCTime)
import qualified Network.HTTP.Client as NC

-- | Access Key type.
newtype AccessKey = AccessKey {unAccessKey :: Text}
  deriving stock (Show)
  deriving newtype (Eq, IsString, Semigroup, Monoid)

-- | Secret Key type - has a show instance that does not print the value.
newtype SecretKey = SecretKey {unSecretKey :: BA.ScrubbedBytes}
  deriving stock (Show)
  deriving newtype (Eq, IsString, Semigroup, Monoid)

-- | Session Token type - has a show instance that does not print the value.
newtype SessionToken = SessionToken {unSessionToken :: BA.ScrubbedBytes}
  deriving stock (Show)
  deriving newtype (Eq, IsString, Semigroup, Monoid)

-- | Object storage credential data type. It has support for the optional
-- [SessionToken](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_use-resources.html)
-- for using temporary credentials requested via STS.
--
-- The show instance for this type does not print the value of secrets for
-- security.
--
-- @since 1.7.0
data CredentialValue = CredentialValue
  { cvAccessKey :: AccessKey,
    cvSecretKey :: SecretKey,
    cvSessionToken :: Maybe SessionToken
  }
  deriving stock (Eq, Show)

scrubbedToText :: BA.ScrubbedBytes -> Text
scrubbedToText =
  let b2t :: ByteString -> Text
      b2t = decodeUtf8
      s2b :: BA.ScrubbedBytes -> ByteString
      s2b = BA.convert
   in b2t . s2b

-- | Convert a 'CredentialValue' to a text tuple. Use this to output the
-- credential to files or other programs.
credentialValueText :: CredentialValue -> (Text, Text, Maybe Text)
credentialValueText cv =
  ( coerce $ cvAccessKey cv,
    (scrubbedToText . coerce) $ cvSecretKey cv,
    scrubbedToText . coerce <$> cvSessionToken cv
  )

-- | Endpoint represented by host, port and TLS enabled flag.
type Endpoint = (ByteString, Int, Bool)

-- | Typeclass for STS credential providers.
--
-- @since 1.7.0
class STSCredentialProvider p where
  retrieveSTSCredentials ::
    p ->
    -- | STS Endpoint (host, port, isSecure)
    Endpoint ->
    NC.Manager ->
    IO (CredentialValue, ExpiryTime)
  getSTSEndpoint :: p -> Maybe Text

-- | 'ExpiryTime' represents a time at which a credential expires.
newtype ExpiryTime = ExpiryTime {unExpiryTime :: UTCTime}
  deriving stock (Show)
  deriving newtype (Eq)
