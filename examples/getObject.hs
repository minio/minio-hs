#!/usr/bin/env stack
-- stack --resolver lts-6.27 runghc --package minio-hs


{-# Language OverloadedStrings #-}
import           Network.Minio

import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
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
      bucket = "krisis"
      object = "fail.out"
  res <- runResourceT $ runMinio minioPlayCI $ do
    (_, src) <- getObject bucket object [] []
    (src C.$$+- CB.sinkLbs)

  print res
