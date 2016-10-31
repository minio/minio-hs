module Main where

-- import qualified Network.HTTP.Conduit as NC

import Protolude

import Network.Minio.API

main :: IO ()
main = do
  mc <- connect defaultConnectInfo
  res <- runMinio mc $ getService
  print res
