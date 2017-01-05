module Network.Minio
  ( module Exports
  , fGetObject
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

import System.FilePath
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB

import Lib.Prelude

import Network.Minio.Data
import Network.Minio.S3API

fGetObject :: Bucket -> Object -> FilePath -> Minio ()
fGetObject bucket object fp = do
  (_, src) <- getObject bucket object [] []
  src C.$$+- CB.sinkFileCautious fp
