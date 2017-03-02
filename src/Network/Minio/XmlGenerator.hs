--
-- Minio Haskell SDK, (C) 2017 Minio, Inc.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

module Network.Minio.XmlGenerator
  ( mkCreateBucketConfig
  , mkCompleteMultipartUploadRequest
  ) where


import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import qualified Data.Text as T
import           Text.XML

import           Lib.Prelude

import           Network.Minio.Data


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
mkCompleteMultipartUploadRequest :: [PartTuple] -> ByteString
mkCompleteMultipartUploadRequest partInfo =
  LBS.toStrict $ renderLBS def cmur
  where
    root = Element "CompleteMultipartUpload" M.empty $
           map (NodeElement . mkPart) partInfo
    mkPart (n, etag) = Element "Part" M.empty
                               [ NodeElement $ Element "PartNumber" M.empty
                                 [NodeContent $ T.pack $ show n]
                               , NodeElement $ Element "ETag" M.empty
                                 [NodeContent etag]
                               ]
    cmur = Document (Prologue [] Nothing []) root []
