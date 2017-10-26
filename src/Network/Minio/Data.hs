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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module Network.Minio.Data where

import           Control.Monad.Base
import qualified Control.Monad.Catch          as MC
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource

import qualified Data.ByteString              as B
import           Data.Default                 (Default (..))
import qualified Data.Map                     as Map
import qualified Data.Text                    as T
import           Data.Time                    (defaultTimeLocale, formatTime)
import           Network.HTTP.Client          (defaultManagerSettings)
import qualified Network.HTTP.Conduit         as NC
import           Network.HTTP.Types           (Header, Method, Query, ByteRange, hRange)
import qualified Network.HTTP.Types           as HT
import           Network.Minio.Errors
import           Text.XML

import           GHC.Show                     (Show (..))

import           Lib.Prelude


-- | max obj size is 5TiB
maxObjectSize :: Int64
maxObjectSize = 5 * 1024 * 1024 * oneMiB

-- | minimum size of parts used in multipart operations.
minPartSize :: Int64
minPartSize = 64 * oneMiB

oneMiB :: Int64
oneMiB = 1024 * 1024

-- | maximum number of parts that can be uploaded for a single object.
maxMultipartParts :: Int64
maxMultipartParts = 10000

-- TODO: Add a type which provides typed constants for region.  this
-- type should have a IsString instance to infer the appropriate
-- constant.
-- | awsRegionMap - library constant
awsRegionMap :: Map.Map Text Text
awsRegionMap = Map.fromList [
      ("us-east-1", "s3.amazonaws.com")
    , ("us-east-2", "s3-us-east-2.amazonaws.com")
    , ("us-west-1", "s3-us-west-1.amazonaws.com")
    , ("us-east-2", "s3-us-west-2.amazonaws.com")
    , ("ca-central-1", "s3-ca-central-1.amazonaws.com")
    , ("ap-south-1", "s3-ap-south-1.amazonaws.com")
    , ("ap-northeast-1", "s3-ap-northeast-1.amazonaws.com")
    , ("ap-northeast-2", "s3-ap-northeast-2.amazonaws.com")
    , ("ap-southeast-1", "s3-ap-southeast-1.amazonaws.com")
    , ("ap-southeast-2", "s3-ap-southeast-2.amazonaws.com")
    , ("eu-west-1", "s3-eu-west-1.amazonaws.com")
    , ("eu-west-2", "s3-eu-west-2.amazonaws.com")
    , ("eu-central-1", "s3-eu-central-1.amazonaws.com")
    , ("sa-east-1", "s3-sa-east-1.amazonaws.com")
  ]

-- | Connection Info data type. To create a 'ConnectInfo' value, use one
-- of the provided smart constructors or override fields of the
-- Default instance.
data ConnectInfo = ConnectInfo {
    connectHost               :: Text
  , connectPort               :: Int
  , connectAccessKey          :: Text
  , connectSecretKey          :: Text
  , connectIsSecure           :: Bool
  , connectRegion             :: Region
  , connectAutoDiscoverRegion :: Bool
  } deriving (Eq, Show)

-- | Connects to a Minio server located at @localhost:9000@ with access
-- key /minio/ and secret key /minio123/. It is over __HTTP__ by
-- default.
instance Default ConnectInfo where
  def = ConnectInfo "localhost" 9000 "minio" "minio123" False "us-east-1" True

-- | Default AWS ConnectInfo. Connects to "us-east-1". Credentials
-- should be supplied before use, for e.g.:
--
-- > awsCI {
-- >   connectAccessKey = "my-access-key"
-- > , connectSecretKey = "my-secret-key"
-- > }
awsCI :: ConnectInfo
awsCI = def {
    connectHost = "s3.amazonaws.com"
  , connectPort = 443
  , connectAccessKey = ""
  , connectSecretKey = ""
  , connectIsSecure = True
  }

-- | AWS ConnectInfo with a specified region. It can optionally
-- disable the automatic discovery of a bucket's region via the
-- Boolean argument.
--
-- > awsWithRegionCI "us-west-1" False {
-- >   connectAccessKey = "my-access-key"
-- > , connectSecretKey = "my-secret-key"
-- > }
--
-- This restricts all operations to the "us-west-1" region and does
-- not perform any bucket location requests.
awsWithRegionCI :: Region -> Bool -> ConnectInfo
awsWithRegionCI region autoDiscoverRegion =
  let host = maybe "s3.amazonaws.com" identity $
             Map.lookup region awsRegionMap
  in awsCI {
      connectHost = host
    , connectRegion = region
    , connectAutoDiscoverRegion = autoDiscoverRegion
    }


-- | <https://play.minio.io:9000 Minio Play Server>
-- ConnectInfo. Credentials are already filled in.
minioPlayCI :: ConnectInfo
minioPlayCI = def {
    connectHost = "play.minio.io"
  , connectPort = 9000
  , connectAccessKey = "Q3AM3UQ867SPQQA43P2F"
  , connectSecretKey = "zuf+tfteSlswRu7BJ86wekitnifILbZam1KYY3TG"
  , connectIsSecure = True
  , connectAutoDiscoverRegion = False
  }

-- | ConnectInfo for Minio server. Takes hostname, port and a Boolean
-- to enable TLS.
--
-- > minioCI "minio.example.com" 9000 True {
-- >   connectAccessKey = "my-access-key"
-- > , connectSecretKey = "my-secret-key"
-- > }
--
-- This connects to a Minio server at the given hostname and port over
-- HTTPS.
minioCI :: Text -> Int -> Bool -> ConnectInfo
minioCI host port isSecure = def {
    connectHost = host
  , connectPort = port
  , connectRegion = "us-east-1"
  , connectIsSecure = isSecure
  , connectAutoDiscoverRegion = False
  }

-- |
-- Represents a bucket in the object store
type Bucket = Text

-- |
-- Represents an object name
type Object = Text

-- |
-- Represents a region
-- TODO: This could be a Sum Type with all defined regions for AWS.
type Region = Text

-- | A type alias to represent an Entity-Tag returned by S3-compatible
-- APIs.
type ETag = Text

-- |
-- BucketInfo returned for list buckets call
data BucketInfo = BucketInfo {
    biName         :: Bucket
  , biCreationDate :: UTCTime
  } deriving (Show, Eq)

-- | A type alias to represent a part-number for multipart upload
type PartNumber = Int16

-- | A type alias to represent an upload-id for multipart upload
type UploadId = Text

-- | A type to represent a part-number and etag.
type PartTuple = (PartNumber, ETag)

-- | Represents result from a listing of object parts of an ongoing
-- multipart upload.
data ListPartsResult = ListPartsResult {
    lprHasMore  :: Bool
  , lprNextPart :: Maybe Int
  , lprParts    :: [ObjectPartInfo]
 } deriving (Show, Eq)


-- | Represents information about an object part in an ongoing
-- multipart upload.
data ObjectPartInfo = ObjectPartInfo {
    opiNumber  :: PartNumber
  , opiETag    :: ETag
  , opiSize    :: Int64
  , opiModTime :: UTCTime
  } deriving (Show, Eq)

-- | Represents result from a listing of incomplete uploads to a
-- bucket.
data ListUploadsResult = ListUploadsResult {
    lurHasMore    :: Bool
  , lurNextKey    :: Maybe Text
  , lurNextUpload :: Maybe Text
  , lurUploads    :: [(Object, UploadId, UTCTime)]
  , lurCPrefixes  :: [Text]
  } deriving (Show, Eq)

-- | Represents information about a multipart upload.
data UploadInfo = UploadInfo {
    uiKey      :: Object
  , uiUploadId :: UploadId
  , uiInitTime :: UTCTime
  , uiSize     :: Int64
  } deriving (Show, Eq)

-- | Represents result from a listing of objects in a bucket.
data ListObjectsResult = ListObjectsResult {
    lorHasMore   :: Bool
  , lorNextToken :: Maybe Text
  , lorObjects   :: [ObjectInfo]
  , lorCPrefixes :: [Text]
  } deriving (Show, Eq)

-- | Represents result from a listing of objects version 1 in a bucket.
data ListObjectsV1Result = ListObjectsV1Result {
    lorHasMore'   :: Bool
  , lorNextMarker :: Maybe Text
  , lorObjects'   :: [ObjectInfo]
  , lorCPrefixes' :: [Text]
  } deriving (Show, Eq)

-- | Represents information about an object.
data ObjectInfo = ObjectInfo {
    oiObject  :: Object
  , oiModTime :: UTCTime
  , oiETag    :: ETag
  , oiSize    :: Int64
  } deriving (Show, Eq)

-- | Represents source object in server-side copy object
data SourceInfo = SourceInfo {
    srcBucket            :: Text
  , srcObject            :: Text
  , srcRange             :: Maybe (Int64, Int64)
  , srcIfMatch           :: Maybe Text
  , srcIfNoneMatch       :: Maybe Text
  , srcIfModifiedSince   :: Maybe UTCTime
  , srcIfUnmodifiedSince :: Maybe UTCTime
  } deriving (Show, Eq)  

instance Default SourceInfo where
  def = SourceInfo "" "" def def def def def

-- | Represents destination object in server-side copy object
data DestinationInfo = DestinationInfo {
  dstBucket   :: Text
  , dstObject :: Text
  } deriving (Show, Eq)

instance Default DestinationInfo where
  def = DestinationInfo "" ""

data GetObjectOptions = GetObjectOptions {
    -- | [ByteRangeFromTo 0 9] means first ten bytes of the source object.
    gooRange :: Maybe ByteRange
  , gooIfMatch :: Maybe ETag
  , gooIfNoneMatch :: Maybe ETag
  , gooIfUnmodifiedSince :: Maybe UTCTime
  , gooIfModifiedSince :: Maybe UTCTime
  } deriving (Show, Eq)

instance Default GetObjectOptions where
  def = GetObjectOptions def def def def def

gooToHeaders :: GetObjectOptions -> [HT.Header]
gooToHeaders goo = rangeHdr ++ zip names values
  where
    names = ["If-Match",
             "If-None-Match",
             "If-Unmodified-Since",
             "If-Modified-Since"]
    values = mapMaybe (fmap encodeUtf8 . (goo &))
             [gooIfMatch, gooIfNoneMatch,
              fmap formatRFC1123 . gooIfUnmodifiedSince,
              fmap formatRFC1123 . gooIfModifiedSince]
    rangeHdr = maybe [] (\a -> [(hRange, HT.renderByteRanges [a])])
               $ gooRange goo


-- | A data-type for events that can occur in the object storage
-- server. Reference:
-- https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html#supported-notification-event-types
data Event = ObjectCreated
           | ObjectCreatedPut
           | ObjectCreatedPost
           | ObjectCreatedCopy
           | ObjectCreatedMultipartUpload
           | ObjectRemoved
           | ObjectRemovedDelete
           | ObjectRemovedDeleteMarkerCreated
           | ReducedRedundancyLostObject
           deriving (Eq)

instance Show Event where
  show ObjectCreated                    = "s3:ObjectCreated:*"
  show ObjectCreatedPut                 = "s3:ObjectCreated:Put"
  show ObjectCreatedPost                = "s3:ObjectCreated:Post"
  show ObjectCreatedCopy                = "s3:ObjectCreated:Copy"
  show ObjectCreatedMultipartUpload     = "s3:ObjectCreated:MultipartUpload"
  show ObjectRemoved                    = "s3:ObjectRemoved:*"
  show ObjectRemovedDelete              = "s3:ObjectRemoved:Delete"
  show ObjectRemovedDeleteMarkerCreated = "s3:ObjectRemoved:DeleteMarkerCreated"
  show ReducedRedundancyLostObject      = "s3:ReducedRedundancyLostObject"

textToEvent :: Text -> Maybe Event
textToEvent t = case t of
  "s3:ObjectCreated:*"                   -> Just ObjectCreated
  "s3:ObjectCreated:Put"                 -> Just ObjectCreatedPut
  "s3:ObjectCreated:Post"                -> Just ObjectCreatedPost
  "s3:ObjectCreated:Copy"                -> Just ObjectCreatedCopy
  "s3:ObjectCreated:MultipartUpload"     -> Just ObjectCreatedMultipartUpload
  "s3:ObjectRemoved:*"                   -> Just ObjectRemoved
  "s3:ObjectRemoved:Delete"              -> Just ObjectRemovedDelete
  "s3:ObjectRemoved:DeleteMarkerCreated" -> Just ObjectRemovedDeleteMarkerCreated
  "s3:ReducedRedundancyLostObject"       -> Just ReducedRedundancyLostObject
  _                                      -> Nothing


data Filter = Filter
  { fFilter :: FilterKey
  } deriving (Show, Eq)

instance Default Filter where
  def = Filter def

data FilterKey = FilterKey
  { fkKey :: FilterRules
  } deriving (Show, Eq)

instance Default FilterKey where
  def = FilterKey def

data FilterRules = FilterRules
  { frFilterRules :: [FilterRule]
  } deriving (Show, Eq)

instance Default FilterRules where
  def = FilterRules []

-- | A filter rule that can act based on the suffix or prefix of an
-- object. As an example, let's create two filter rules:
--
--    > let suffixRule = FilterRule "suffix" ".jpg"
--    > let prefixRule = FilterRule "prefix" "images/"
--
-- The `suffixRule` restricts the notification to be triggered only
-- for objects having a suffix of ".jpg", and the `prefixRule`
-- restricts it to objects having a prefix of "images/".
data FilterRule = FilterRule
  { frName  :: Text
  , frValue :: Text
  } deriving (Show, Eq)

type Arn = Text

-- | A data-type representing the configuration for a particular
-- notification system. It could represent a Queue, Topic or Lambda
-- Function configuration.
data NotificationConfig = NotificationConfig
  { ncId     :: Text
  , ncArn    :: Arn
  , ncEvents :: [Event]
  , ncFilter :: Filter
  } deriving (Show, Eq)

-- | A data-type to represent bucket notification configuration. It is
-- a collection of queue, topic or lambda function configurations. The
-- structure of the types follow closely the XML representation
-- described at
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTnotification.html>
data Notification = Notification
  { nQueueConfigurations         :: [NotificationConfig]
  , nTopicConfigurations         :: [NotificationConfig]
  , nCloudFunctionConfigurations :: [NotificationConfig]
  } deriving (Eq, Show)

instance Default Notification where
  def = Notification [] [] []

-- | Represents different kinds of payload that are used with S3 API
-- requests.
data Payload = PayloadBS ByteString
             | PayloadH Handle
                        Int64 -- offset
                        Int64 -- size

instance Default Payload where
  def = PayloadBS ""

data RequestInfo = RequestInfo {
    riMethod        :: Method
  , riBucket        :: Maybe Bucket
  , riObject        :: Maybe Object
  , riQueryParams   :: Query
  , riHeaders       :: [Header]
  , riPayload       :: Payload
  , riPayloadHash   :: Maybe ByteString
  , riRegion        :: Maybe Region
  , riNeedsLocation :: Bool
  }

instance Default RequestInfo where
  def = RequestInfo HT.methodGet def def def def def Nothing def True

getPathFromRI :: RequestInfo -> ByteString
getPathFromRI ri =
  let
    b = riBucket ri
    o = riObject ri
    segments = map toS $ catMaybes $ b : bool [] [o] (isJust b)
  in
    B.concat ["/", B.intercalate "/" segments]

-- | Time to expire for a presigned URL. It interpreted as a number of
-- seconds. The maximum duration that can be specified is 7 days.
type UrlExpiry = Int

type RegionMap = Map.Map Bucket Region

newtype Minio a = Minio {
  unMinio :: ReaderT MinioConn (StateT RegionMap (ResourceT IO)) a
  }
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader MinioConn
    , MonadState RegionMap
    , MonadThrow
    , MonadCatch
    , MonadBase IO
    , MonadResource
    )

instance MonadBaseControl IO Minio where
  type StM Minio a = (a, RegionMap)
  liftBaseWith f = Minio $ liftBaseWith $ \q -> f (q . unMinio)
  restoreM = Minio . restoreM

-- | MinioConn holds connection info and a connection pool
data MinioConn = MinioConn {
    mcConnInfo    :: ConnectInfo
  , mcConnManager :: NC.Manager
  }

-- | Takes connection information and returns a connection object to
-- be passed to 'runMinio'
connect :: ConnectInfo -> IO MinioConn
connect ci = do
  let settings = bool defaultManagerSettings NC.tlsManagerSettings $
        connectIsSecure ci
  mgr <- NC.newManager settings
  return $ MinioConn ci mgr

-- | Run the Minio action and return the result or an error.
runMinio :: ConnectInfo -> Minio a -> IO (Either MinioErr a)
runMinio ci m = do
  conn <- liftIO $ connect ci
  runResourceT . flip evalStateT Map.empty . flip runReaderT conn . unMinio $
    fmap Right m `MC.catches`
    [ MC.Handler handlerServiceErr
    , MC.Handler handlerHE
    , MC.Handler handlerFE
    , MC.Handler handlerValidation
    ]
  where
    handlerServiceErr = return . Left . MErrService
    handlerHE = return . Left . MErrHTTP
    handlerFE = return . Left . MErrIO
    handlerValidation = return . Left . MErrValidation

s3Name :: Text -> Name
s3Name s = Name s (Just "http://s3.amazonaws.com/doc/2006-03-01/") Nothing

-- | Format as per RFC 1123.
formatRFC1123 :: UTCTime -> T.Text
formatRFC1123 = T.pack . formatTime defaultTimeLocale "%a, %d %b %Y %X %Z"
