#!/usr/bin/env stack
-- stack --resolver lts-6.27 runghc --package minio-hs


{-# Language OverloadedStrings #-}
import           Network.Minio

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
  firstRegionE <- runResourceT $ runMinio minioPlayCI $ do
    buckets <- getService
    getLocation $ biName $ head buckets
  print firstRegionE
