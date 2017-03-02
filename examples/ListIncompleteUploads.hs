#!/usr/bin/env stack
-- stack --resolver lts-6.27 runghc --package minio-hs

{-# Language OverloadedStrings #-}
import Network.Minio

import Data.Conduit (($$))
import Data.Conduit.Combinators (sinkList)
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

  -- Performs a recursive listing of incomplete uploads under bucket "test"
  -- on a local minio server.
  res <- runResourceT $ runMinio minioPlayCI $ do
    listIncompleteUploads bucket Nothing True $$ sinkList
  print res

  {-
    Following is the output of the above program on a local Minio server.

    Right [UploadInfo { uiKey = "go1.6.2.linux-amd64.tar.gz"
                      , uiUploadId = "063eb592-bdd7-4a0c-be48-34fb3ceb63e2"
                      , uiInitTime = 2017-03-01 10:16:25.698 UTC
                      , uiSize = 17731794
                      }
          ]
  -}
