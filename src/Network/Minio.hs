module Network.Minio
  ( module Exports
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
