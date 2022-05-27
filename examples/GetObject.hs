#!/usr/bin/env stack
-- stack --resolver lts-14.11 runghc --package minio-hs

--
-- MinIO Haskell SDK, (C) 2017, 2018 MinIO, Inc.
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

import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import Network.Minio
import Prelude

-- | The following example uses minio's play server at
-- https://play.min.io.  The endpoint and associated
-- credentials are provided via the libary constant,
--
-- > minioPlayCI :: ConnectInfo
main :: IO ()
main = do
  let bucket = "my-bucket"
      object = "my-object"
  res <- runMinio minioPlayCI $ do
    src <- getObject bucket object defaultGetObjectOptions
    C.connect (gorObjectStream src) $ CB.sinkFileCautious "/tmp/my-object"

  case res of
    Left e -> putStrLn $ "getObject failed." ++ show e
    Right _ -> putStrLn "getObject succeeded."
