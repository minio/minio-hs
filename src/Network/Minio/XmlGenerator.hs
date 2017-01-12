module Network.Minio.XmlGenerator
  ( mkCreateBucketConfig
  ) where

import Lib.Prelude

import qualified Data.ByteString.Lazy as LBS
import Text.XML
import qualified Data.Map as M

import Network.Minio.Data


mkCreateBucketConfig :: Location -> ByteString
mkCreateBucketConfig location = LBS.toStrict $ renderLBS def bucketConfig
  where
      s3Element n = Element (s3Name n) M.empty
      root =  s3Element "CreateBucketConfiguration"
        [ NodeElement $ s3Element "LocationConstraint"
          [ NodeContent location]
        ]
      bucketConfig = Document (Prologue [] Nothing []) root []
