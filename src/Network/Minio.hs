module Network.Minio
  ( module Exports
  , fGetObject
  , fPutObject
  ) where

{-
This module exports the high-level Minio API for object storage.
-}

import Network.Minio.S3API as
  Exports (
    getService
  , getLocation
  )

import Network.Minio.Data as
  Exports (
    runMinio
  , defaultConnectInfo
  , connect
  , ConnectInfo(..)
  )

-- import System.FilePath (FilePath)
import qualified System.IO as IO
import qualified Data.Conduit as C
import qualified Control.Monad.Trans.Resource as R
import qualified Data.Conduit.Binary as CB

import Lib.Prelude

import Network.Minio.Data
import Network.Minio.S3API
import Network.Minio.Utils

fGetObject :: Bucket -> Object -> FilePath -> Minio ()
fGetObject bucket object fp = do
  (_, src) <- getObject bucket object [] []
  src C.$$+- CB.sinkFileCautious fp

fPutObject :: Bucket -> Object -> FilePath -> Minio ()
fPutObject bucket object fp = do
  (releaseKey, h) <- allocateReadFile fp

  size <- liftIO $ IO.hFileSize h
  putObject bucket object [] 0 (fromIntegral size) h

  -- release file handle
  R.release releaseKey
