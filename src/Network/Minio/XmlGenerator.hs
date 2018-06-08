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
  , mkPutNotificationRequest
  ) where


import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map             as M
import qualified Data.Text            as T
import           Text.XML

import           Lib.Prelude

import           Network.Minio.Data


-- | Create a bucketConfig request body XML
mkCreateBucketConfig :: Text -> Region -> ByteString
mkCreateBucketConfig ns location = LBS.toStrict $ renderLBS def bucketConfig
  where
      s3Element n = Element (s3Name ns n) M.empty
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

-- Simplified XML representation without element attributes.
data XNode = XNode Text [XNode]
           | XLeaf Text Text
  deriving (Eq, Show)

toXML :: Text -> XNode -> ByteString
toXML ns node = LBS.toStrict $ renderLBS def $
  Document (Prologue [] Nothing []) (xmlNode node) []
  where
    xmlNode :: XNode -> Element
    xmlNode (XNode name nodes)   = Element (s3Name ns name) M.empty $
                                   map (NodeElement . xmlNode) nodes
    xmlNode (XLeaf name content) = Element (s3Name ns name) M.empty
                                   [NodeContent content]

class ToXNode a where
  toXNode :: a -> XNode

instance ToXNode Event where
  toXNode = XLeaf "Event" . show

instance ToXNode Notification where
  toXNode (Notification qc tc lc) = XNode "NotificationConfiguration" $
    map (toXNodesWithArnName "QueueConfiguration" "Queue") qc ++
    map (toXNodesWithArnName "TopicConfiguration" "Topic") tc ++
    map (toXNodesWithArnName "CloudFunctionConfiguration" "CloudFunction") lc

toXNodesWithArnName :: Text -> Text -> NotificationConfig -> XNode
toXNodesWithArnName eltName arnName (NotificationConfig id arn events fRule) =
  XNode eltName $ [XLeaf "Id" id, XLeaf arnName arn] ++ map toXNode events ++
  [toXNode fRule]

instance ToXNode Filter where
  toXNode (Filter (FilterKey (FilterRules rules))) =
    XNode "Filter" [XNode "S3Key" (map getFRXNode rules)]

getFRXNode :: FilterRule -> XNode
getFRXNode (FilterRule n v) = XNode "FilterRule" [ XLeaf "Name" n
                                                 , XLeaf "Value" v
                                                 ]

mkPutNotificationRequest :: Text -> Notification -> ByteString
mkPutNotificationRequest ns = toXML ns . toXNode
