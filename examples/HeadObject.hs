#!/usr/bin/env stack
-- stack --resolver lts-8.5 runghc --package minio-hs

--
-- Minio Haskell SDK, (C) 2017 Minio, Inc.
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

{-# Language OverloadedStrings #-}
import Network.Minio.S3API
import Network.Minio

import Prelude

-- | The following example uses minio's play server at
-- https://play.minio.io:9000.  The endpoint and associated
-- credentials are provided via the libary constant,
--
-- > minioPlayCI :: ConnectInfo
--

main :: IO ()
main = do
  let
      bucket = "test"
      object = "passwd"
  res <- runMinio minioPlayCI $
    headObject bucket object

  case res of
    Left e -> putStrLn $ "headObject failed." ++ show e
    Right objInfo -> putStrLn $ "headObject succeeded." ++ show objInfo
