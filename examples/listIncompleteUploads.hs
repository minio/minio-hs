#!/usr/bin/env stack
-- stack --resolver lts-6.27 runghc --package minio-hs

{-# Language OverloadedStrings #-}
import           Network.Minio

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import           Data.Default (Default(..))
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

  -- Performs a recursive listing of incomplete uploads under bucket "test"
  -- on a local minio server.
  res <- runResourceT $ runMinio def $ do
    listIncompleteUploads bucket Nothing True C.$$ CC.sinkList
  print res

  {-
    Following is the output of the above program on a local Minio server.

    Right [UploadInfo {uiKey = "multipartObj", uiUploadId = "0ff9ccb9-d7ff-4def-9a98-571abefd7e2a", uiInitTime = 2017-02-10 12:19:04.951 UTC}]
  -}
