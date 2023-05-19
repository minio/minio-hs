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

module Network.Minio.Credentials
  ( CredentialValue (..),
    credentialValueText,
    STSCredentialProvider (..),
    AccessKey (..),
    SecretKey (..),
    SessionToken (..),
    ExpiryTime (..),
    STSCredentialStore,
    initSTSCredential,
    getSTSCredential,
    Creds (..),
    getCredential,
    Endpoint,

    -- * STS Assume Role
    defaultSTSAssumeRoleOptions,
    STSAssumeRole (..),
    STSAssumeRoleOptions (..),
  )
where

import Data.Time (diffUTCTime, getCurrentTime)
import qualified Network.HTTP.Client as NC
import Network.Minio.Credentials.AssumeRole
import Network.Minio.Credentials.Types
import qualified UnliftIO.MVar as M

data STSCredentialStore = STSCredentialStore
  { cachedCredentials :: M.MVar (CredentialValue, ExpiryTime),
    refreshAction :: Endpoint -> NC.Manager -> IO (CredentialValue, ExpiryTime)
  }

initSTSCredential :: (STSCredentialProvider p) => p -> IO STSCredentialStore
initSTSCredential p = do
  let action = retrieveSTSCredentials p
  -- start with dummy credential, so that refresh happens for first request.
  now <- getCurrentTime
  mvar <- M.newMVar (CredentialValue mempty mempty mempty, coerce now)
  return $
    STSCredentialStore
      { cachedCredentials = mvar,
        refreshAction = action
      }

getSTSCredential :: STSCredentialStore -> Endpoint -> NC.Manager -> IO (CredentialValue, Bool)
getSTSCredential store ep mgr = M.modifyMVar (cachedCredentials store) $ \cc@(v, expiry) -> do
  now <- getCurrentTime
  if diffUTCTime now (coerce expiry) > 0
    then do
      res <- refreshAction store ep mgr
      return (res, (fst res, True))
    else return (cc, (v, False))

data Creds
  = CredsStatic CredentialValue
  | CredsSTS STSCredentialStore

getCredential :: Creds -> Endpoint -> NC.Manager -> IO CredentialValue
getCredential (CredsStatic v) _ _ = return v
getCredential (CredsSTS s) ep mgr = fst <$> getSTSCredential s ep mgr
