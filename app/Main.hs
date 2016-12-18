module Main where

-- import qualified Network.HTTP.Conduit as NC

import Protolude

import Network.Minio.Data

import Network.Minio.API
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)

main :: IO ()
main = do
  mc <- connect defaultConnectInfo
  res <- runResourceT $ runMinio mc $ getService
  print $ rpiStatus <$> res
