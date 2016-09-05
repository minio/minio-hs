module Main where

import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Conduit as NC

import Protolude

import Network.Minio.API
import Network.Minio.Data

main :: IO ()
main = do
  resp <- minioExecute mc req
  print $ NC.responseStatus resp
  print $ NC.responseHeaders resp
  print $ NC.responseBody resp
  where
    mc = MinioClient "localhost" 9000 "abcd1" "abcd1234" False "us-east-1"
    req = RequestInfo HT.methodGet Nothing Nothing [] [] "" ""
