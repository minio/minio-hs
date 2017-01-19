module Network.Minio.XmlGenerator
  ( mkCreateBucketConfig
  , mkCompleteMultipartUploadRequest
  ) where

import Lib.Prelude

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Text.XML
import qualified Data.Map as M

import Network.Minio.Data


-- | Create a bucketConfig request body XML
mkCreateBucketConfig :: Region -> ByteString
mkCreateBucketConfig location = LBS.toStrict $ renderLBS def bucketConfig
  where
      s3Element n = Element (s3Name n) M.empty
      root = s3Element "CreateBucketConfiguration"
        [ NodeElement $ s3Element "LocationConstraint"
          [ NodeContent location]
        ]
      bucketConfig = Document (Prologue [] Nothing []) root []

-- | Create a completeMultipartUpload request body XML
mkCompleteMultipartUploadRequest :: [PartInfo] -> ByteString
mkCompleteMultipartUploadRequest partInfo =
  LBS.toStrict $ renderLBS def cmur
  where
    root = Element "CompleteMultipartUpload" M.empty $
           map (NodeElement . mkPart) partInfo
    mkPart (PartInfo n etag) = Element "Part" M.empty
                               [ NodeElement $ Element "PartNumber" M.empty
                                 [NodeContent $ T.pack $ show n]
                               , NodeElement $ Element "ETag" M.empty
                                 [NodeContent etag]
                               ]
    cmur = Document (Prologue [] Nothing []) root []
