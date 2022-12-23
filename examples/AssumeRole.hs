--
-- MinIO Haskell SDK, (C) 2022 MinIO, Inc.
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

import Network.Minio.Credentials
import Prelude

main :: IO ()
main = do
  res <-
    retrieveCredentials
      $ STSAssumeRole
        "https://play.min.io"
        ( CredentialValue
            "Q3AM3UQ867SPQQA43P2F"
            "zuf+tfteSlswRu7BJ86wekitnifILbZam1KYY3TG"
            Nothing
        )
      $ defaultSTSAssumeRoleOptions {saroLocation = Just "us-east-1"}
  print res
