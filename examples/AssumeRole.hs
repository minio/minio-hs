--
-- MinIO Haskell SDK, (C) 2023 MinIO, Inc.
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
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Network.Minio
import Prelude

main :: IO ()
main = do
  -- Use play credentials for example.
  let assumeRole =
        STSAssumeRole
          ( CredentialValue
              "Q3AM3UQ867SPQQA43P2F"
              "zuf+tfteSlswRu7BJ86wekitnifILbZam1KYY3TG"
              Nothing
          )
          $ defaultSTSAssumeRoleOptions
            { saroLocation = Just "us-east-1",
              saroEndpoint = Just "https://play.min.io:9000"
            }

  -- Retrieve temporary credentials and print them.
  cv <- requestSTSCredential assumeRole
  print $ "Temporary credentials" ++ show (credentialValueText $ fst cv)
  print $ "Expiry" ++ show (snd cv)

  -- Configure 'ConnectInfo' to request temporary credentials on demand.
  ci <- setSTSCredential assumeRole "https://play.min.io"
  res <- runMinio ci $ do
    buckets <- listBuckets
    liftIO $ print $ "Top 5 buckets: " ++ show (take 5 buckets)
  print res
