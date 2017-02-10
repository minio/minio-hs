#!/usr/bin/env stack
-- stack --resolver lts-6.27 runghc --package minio-hs

{-# Language OverloadedStrings #-}
import           Network.Minio

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
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
    bucket = "test"

  -- Performs a recursive listing of all objects under bucket "test"
  -- on play.minio.io.
  res <- runResourceT $ runMinio minioPlayCI $ do
    listObjects bucket Nothing True C.$$ CC.sinkList
  print res

  {-
    Following is the output of the above program on a local Minio server.

    Right [ObjectInfo {oiObject = "ADVANCED.png", oiModTime = 2017-02-10 05:33:24.816 UTC, oiETag = "\"a69f3af6bbb06fe1d42ac910ec30482f\"", oiSize = 94026},ObjectInfo {oiObject = "obj", oiModTime = 2017-02-10 08:49:26.777 UTC, oiETag = "\"715a872a253a3596652c1490081b4b6a-1\"", oiSize = 15728640}]
  -}
