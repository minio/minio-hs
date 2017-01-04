module Main where

import Protolude

import Network.Minio

-- import Network.Minio.S3API
import Control.Monad.Trans.Resource (runResourceT)
-- import qualified Data.Conduit as C
-- import qualified Data.Conduit.List as CL
-- import qualified Network.HTTP.Conduit as NC

main :: IO ()
main = do
  mc <- connect defaultConnectInfo
  t <- runResourceT $ runMinio mc $ do
    res <- getService
    print res
    -- case res of
    --   Left e -> print e
    --   Right res1 -> mapM_ print res1
    -- liftIO $ print $ NC.responseStatus res
    -- liftIO $ print $ NC.responseHeaders res
    --    liftIO print $ NC.responseHeaders <$> res
    -- let bodyE = NC.responseBody <$> res
    -- case bodyE of
    --   Left x -> print x
    --   Right body -> body C.$$+- CL.mapM_ putStrLn
    -- body <- NC.responseBody <$> res
    -- NC.responseBody res C.$$+- CL.mapM_ putStrLn
    res <- getLocation "test1"
    print res

  print "After runResourceT"
  print t
