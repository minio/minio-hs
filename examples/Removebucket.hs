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

main :: IO ()
main = do
  let
      bucket = "my-bucket"
  res <- runResourceT $ runMinio minioPlayCI $ do
    removeBucket bucket
  print res
