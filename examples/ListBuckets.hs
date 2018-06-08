#!/usr/bin/env stack
-- stack --resolver lts-11.1 runghc --package minio-hs

--
-- Minio Haskell SDK, (C) 2017, 2018 Minio, Inc.
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
import           Network.Minio

import           Control.Monad.IO.Class (liftIO)
import           Prelude

-- | The following example uses minio's play server at
-- https://play.minio.io:9000.  The endpoint and associated
-- credentials are provided via the libary constant,
--
-- > minioPlayCI :: ConnectInfo
--

-- This example list buckets that belongs to the user and returns
-- region of the first bucket returned.
main :: IO ()
main = do
  firstRegionE <- runMinio minioPlayCI $ do
    buckets <- listBuckets
    liftIO $ print $ "Top 5 buckets: " ++ show (take 5 buckets)
    getLocation $ biName $ head buckets
  print firstRegionE
