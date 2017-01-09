module Network.Minio.XmlGenerator
  ( mkCreateBucketConfig
  ) where

import Lib.Prelude

import qualified Data.ByteString.Lazy as LBS
import Text.XML
import qualified Data.Map as M

import Network.Minio.Data

mkCreateBucketConfig :: Bucket -> Location -> ByteString
mkCreateBucketConfig bucket location = LBS.toStrict $ renderLBS def bucketConfig
  where
      root = Element (s3Name "CreateBucketConfiguration") M.empty
        [ NodeElement $ Element  "LocationConstraint" M.empty
          [ NodeContent location]
        ]
      bucketConfig = Document (Prologue [] Nothing []) root []
