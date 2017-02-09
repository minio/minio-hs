#!/usr/bin/env stack
-- stack --resolver lts-6.27 runghc --package minio-hs

{-# Language OverloadedStrings #-}
import           Network.Minio

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
      object = "obj"
      mb15 = 15 * 1024 * 1024

  -- Eg 1. Upload a stream of repeating "a" using putObjectFromSource.
  res1 <- runResourceT $ runMinio minioPlayCI $ do
    putObjectFromSource bucket object (CC.repeat "a") (Just mb15)
  case res1 of
    Left e -> putStrLn $ "putObjectFromSource failed." ++ (show e)
    Right () -> putStrLn "putObjectFromSource succeeded."


  -- Eg 2. Upload a file using fPutObject.
  res2 <- runResourceT $ runMinio minioPlayCI $ do
    fPutObject bucket object "path/to/local/file"
  case res2 of
    Left e -> putStrLn $ "fPutObject failed." ++ (show e)
    Right () -> putStrLn "fPutObject succeeded."
