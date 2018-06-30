--
-- Minio Haskell SDK, (C) 2017, 2018 Minio, Inc.
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

import           Control.Concurrent.MVar      (MVar)
import qualified Control.Concurrent.MVar      as M
import           Control.Monad.IO.Unlift      (MonadUnliftIO, UnliftIO (..),
                                               askUnliftIO, withUnliftIO)
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as B
import           Data.CaseInsensitive         (mk)
import qualified Data.Ini                     as Ini
import qualified Data.Map                     as Map
import           Data.String                  (IsString (..))
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Data.Time                    (defaultTimeLocale, formatTime)
import           GHC.Show                     (Show (show))
import           Network.HTTP.Client          (defaultManagerSettings)
import qualified Network.HTTP.Conduit         as NC
import           Network.HTTP.Types           (ByteRange, Header, Method, Query,
                                               hRange)
import qualified Network.HTTP.Types           as HT
import           Network.Minio.Errors
import           System.Directory             (doesFileExist, getHomeDirectory)
import qualified System.Environment           as Env
import           System.FilePath.Posix        (combine)
import           Text.XML
import qualified UnliftIO                     as U

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


instance IsString ConnectInfo where
    fromString str = let req = NC.parseRequest_ str
                     in ConnectInfo
      { connectHost = TE.decodeUtf8 $ NC.host req
                        , connectPort = NC.port req
                        , connectAccessKey = ""
                        , connectSecretKey = ""
                        , connectIsSecure = NC.secure req
                        , connectRegion = ""
                        , connectAutoDiscoverRegion = True
                        }

data Credentials = Credentials { cAccessKey :: Text
                               , cSecretKey :: Text
                               } deriving (Eq, Show)

type Provider = IO (Maybe Credentials)

findFirst :: [Provider] -> Provider
findFirst [] = return Nothing
findFirst (f:fs) = do c <- f
                      maybe (findFirst fs) (return . Just) c

fromAWSConfigFile :: Provider
fromAWSConfigFile = do
    credsE <- runExceptT $ do
        homeDir <- lift $ getHomeDirectory
        let awsCredsFile =  homeDir `combine` ".aws" `combine` "credentials"
        fileExists <- lift $ doesFileExist awsCredsFile
        bool (throwE "FileNotFound") (return ()) fileExists
        ini <- ExceptT $ Ini.readIniFile awsCredsFile
        akey <- ExceptT $ return
                $ Ini.lookupValue "default" "aws_access_key_id" ini
        skey <- ExceptT $ return
                $ Ini.lookupValue "default" "aws_secret_access_key" ini
        return $ Credentials akey skey
    return $ hush credsE

fromAWSEnv :: Provider
fromAWSEnv = runMaybeT $ do
        akey <- MaybeT $ Env.lookupEnv "AWS_ACCESS_KEY_ID"
        skey <- MaybeT $ Env.lookupEnv "AWS_SECRET_ACCESS_KEY"
        return $ Credentials (T.pack akey) (T.pack skey)

fromMinioEnv :: Provider
fromMinioEnv = runMaybeT $ do
    akey <- MaybeT $ Env.lookupEnv "MINIO_ACCESS_KEY"
    skey <- MaybeT $ Env.lookupEnv "MINIO_SECRET_KEY"
    return $ Credentials (T.pack akey) (T.pack skey)

setCredsFrom :: [Provider] -> ConnectInfo -> IO ConnectInfo
setCredsFrom ps ci = do pMay <- findFirst ps
                        maybe
                          (throwIO MErrVMissingCredentials)
                          (return . (flip setCreds ci))
                          pMay

setCreds :: Credentials -> ConnectInfo -> ConnectInfo
setCreds (Credentials accessKey secretKey) connInfo =
    connInfo { connectAccessKey = accessKey
             , connectSecretKey = secretKey
             }

setRegion :: Region -> ConnectInfo -> ConnectInfo
setRegion r connInfo = connInfo { connectRegion = r
                                , connectAutoDiscoverRegion = False
                                }

getHostAddr :: ConnectInfo -> ByteString
getHostAddr ci = if | port == 80 || port == 443 -> toS host
                    | otherwise -> toS $
                                   T.concat [ host, ":" , Lib.Prelude.show port]
  where
    port = connectPort ci
    host = connectHost ci


-- | Default GCS ConnectInfo. Works only for "Simple Migration"
-- use-case with interoperability mode enabled on GCP console. For
-- more information - https://cloud.google.com/storage/docs/migrating
-- Credentials should be supplied before use.
gcsCI :: ConnectInfo
gcsCI = setRegion "us"
        "https://storage.googleapis.com"


-- | Default AWS ConnectInfo. Connects to "us-east-1". Credentials
-- should be supplied before use.
awsCI :: ConnectInfo
awsCI = "https://s3.amazonaws.com"


-- | <https://play.minio.io:9000 Minio Play Server>
-- ConnectInfo. Credentials are already filled in.
minioPlayCI :: ConnectInfo
minioPlayCI = let playCreds = Credentials "Q3AM3UQ867SPQQA43P2F" "zuf+tfteSlswRu7BJ86wekitnifILbZam1KYY3TG"
              in setCreds playCreds
                 $ setRegion "us-east-1"
                 "https://play.minio.io:9000"

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

-- | A type alias to represent an Entity-Tag returned by S3-compatible APIs.
type ETag = Text

-- |
-- Data type represents various options specified for PutObject call.
-- To specify PutObject options use the poo* accessors.
data PutObjectOptions = PutObjectOptions {
  -- | Set a standard MIME type describing the format of the object.
    pooContentType        :: Maybe Text
  -- | Set what content encodings have been applied to the object and thus
  -- what decoding mechanisms must be applied to obtain the media-type
  -- referenced by the Content-Type header field.
  , pooContentEncoding    :: Maybe Text
  -- | Set presentational information for the object.
  , pooContentDisposition :: Maybe Text
  -- | Set to specify caching behavior for the object along the
  -- request/reply chain.
  , pooCacheControl       :: Maybe Text
  -- | Set to describe the language(s) intended for the audience.
  , pooContentLanguage    :: Maybe Text
  -- | Set to 'STANDARD' or 'REDUCED_REDUNDANCY' depending on your
  -- performance needs, storage class is 'STANDARD' by default (i.e
  -- when Nothing is passed).
  , pooStorageClass       :: Maybe Text
  -- | Set user defined metadata to store with the object.
  , pooUserMetadata       :: [(Text, Text)]
  -- | Set number of worker threads used to upload an object.
  , pooNumThreads         :: Maybe Word
  } deriving (Show, Eq)

-- Provide a default instance
defaultPutObjectOptions :: PutObjectOptions
defaultPutObjectOptions = PutObjectOptions Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing

addXAmzMetaPrefix :: Text -> Text
addXAmzMetaPrefix s = do
  if (T.isPrefixOf "x-amz-meta-" s)
    then s
    else T.concat ["x-amz-meta-", s]

mkHeaderFromMetadata :: [(Text, Text)] -> [HT.Header]
mkHeaderFromMetadata = map (\(x, y) -> (mk $ encodeUtf8 $ addXAmzMetaPrefix $ T.toLower x, encodeUtf8 y))

pooToHeaders :: PutObjectOptions -> [HT.Header]
pooToHeaders poo = userMetadata
                   ++ (catMaybes $ map tupToMaybe (zipWith (,) names values))
  where
    tupToMaybe (k, Just v)  = Just (k, v)
    tupToMaybe (_, Nothing) = Nothing

    userMetadata = mkHeaderFromMetadata $ pooUserMetadata poo

    names = ["content-type",
             "content-encoding",
             "content-disposition",
             "content-language",
             "cache-control",
             "x-amz-storage-class"]
    values = map (fmap encodeUtf8 . (poo &))
             [pooContentType, pooContentEncoding,
              pooContentDisposition, pooContentLanguage,
              pooCacheControl, pooStorageClass]


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
    oiObject   :: Object
  , oiModTime  :: UTCTime
  , oiETag     :: ETag
  , oiSize     :: Int64
  , oiMetadata :: Map.Map Text Text
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

defaultSourceInfo :: SourceInfo
defaultSourceInfo = SourceInfo "" "" Nothing Nothing Nothing Nothing Nothing

-- | Represents destination object in server-side copy object
data DestinationInfo = DestinationInfo
                       { dstBucket :: Text
                       , dstObject :: Text
                       } deriving (Show, Eq)

defaultDestinationInfo :: DestinationInfo
defaultDestinationInfo = DestinationInfo "" ""

data GetObjectOptions = GetObjectOptions {
    -- | Set object's data of given offset begin and end,
    -- [ByteRangeFromTo 0 9] means first ten bytes of the source object.
    gooRange             :: Maybe ByteRange
    -- | Set matching ETag condition, GetObject which matches the following
    -- ETag.
  , gooIfMatch           :: Maybe ETag
    -- | Set matching ETag none condition, GetObject which does not match
    -- the following ETag.
  , gooIfNoneMatch       :: Maybe ETag
    -- | Set object unmodified condition, GetObject unmodified since given time.
  , gooIfUnmodifiedSince :: Maybe UTCTime
    -- | Set object modified condition, GetObject modified since given time.
  , gooIfModifiedSince   :: Maybe UTCTime
  } deriving (Show, Eq)

defaultGetObjectOptions :: GetObjectOptions
defaultGetObjectOptions = GetObjectOptions Nothing Nothing Nothing Nothing Nothing

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

defaultFilter :: Filter
defaultFilter = Filter defaultFilterKey

data FilterKey = FilterKey
  { fkKey :: FilterRules
  } deriving (Show, Eq)

defaultFilterKey :: FilterKey
defaultFilterKey = FilterKey defaultFilterRules

data FilterRules = FilterRules
  { frFilterRules :: [FilterRule]
  } deriving (Show, Eq)

defaultFilterRules :: FilterRules
defaultFilterRules = FilterRules []


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

defaultNotification :: Notification
defaultNotification = Notification [] [] []

-- | Represents different kinds of payload that are used with S3 API
-- requests.
data Payload = PayloadBS ByteString
             | PayloadH Handle
                        Int64 -- offset
                        Int64 -- size

defaultPayload :: Payload
defaultPayload = PayloadBS ""

data AdminReqInfo = AdminReqInfo {
    ariMethod      :: Method
  , ariPayloadHash :: Maybe ByteString
  , ariPayload     :: Payload
  , ariPath        :: ByteString
  , ariHeaders     :: [Header]
  , ariQueryParams :: Query
  }

data S3ReqInfo = S3ReqInfo {
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

defaultS3ReqInfo :: S3ReqInfo
defaultS3ReqInfo = S3ReqInfo HT.methodGet Nothing Nothing
                   [] [] defaultPayload Nothing Nothing True

getS3Path :: Maybe Bucket -> Maybe Object -> ByteString
getS3Path b o =
  let segments = map toS $ catMaybes $ b : bool [] [o] (isJust b)
  in
    B.concat ["/", B.intercalate "/" segments]

-- | Time to expire for a presigned URL. It interpreted as a number of
-- seconds. The maximum duration that can be specified is 7 days.
type UrlExpiry = Int

type RegionMap = Map.Map Bucket Region

newtype Minio a = Minio {
  unMinio :: ReaderT MinioConn (ResourceT IO) a
  }
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader MinioConn
    , MonadResource
    )

instance MonadUnliftIO Minio where
  askUnliftIO = Minio $ ReaderT $ \r ->
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . flip runReaderT r . unMinio))

-- | MinioConn holds connection info and a connection pool
data MinioConn = MinioConn
  { mcConnInfo    :: ConnectInfo
  , mcConnManager :: NC.Manager
  , mcRegionMap   :: MVar RegionMap
  }

class HasSvcNamespace env where
  getSvcNamespace :: env -> Text

instance HasSvcNamespace MinioConn where
  getSvcNamespace env = let host = connectHost $ mcConnInfo env
                            in if | host  == "storage.googleapis.com" ->
                                    "http://doc.s3.amazonaws.com/2006-03-01"
                                  | otherwise ->
                                    "http://s3.amazonaws.com/doc/2006-03-01/"

-- | Takes connection information and returns a connection object to
-- be passed to 'runMinio'
connect :: ConnectInfo -> IO MinioConn
connect ci = do
  let settings | connectIsSecure ci = NC.tlsManagerSettings
               | otherwise = defaultManagerSettings
  mgr <- NC.newManager settings
  mkMinioConn ci mgr


runMinioWith :: MinioConn -> Minio a -> IO (Either MinioErr a)
runMinioWith conn m = runResourceT . flip runReaderT conn . unMinio $
    fmap Right m `U.catches`
    [ U.Handler handlerServiceErr
    , U.Handler handlerHE
    , U.Handler handlerFE
    , U.Handler handlerValidation
    ]
  where
    handlerServiceErr = return . Left . MErrService
    handlerHE = return . Left . MErrHTTP
    handlerFE = return . Left . MErrIO
    handlerValidation = return . Left . MErrValidation

mkMinioConn :: ConnectInfo -> NC.Manager -> IO MinioConn
mkMinioConn ci mgr = do
    rMapMVar <- M.newMVar Map.empty
    return $ MinioConn ci mgr rMapMVar

-- | Run the Minio action and return the result or an error.
runMinio :: ConnectInfo -> Minio a -> IO (Either MinioErr a)
runMinio ci m = do
  conn <- connect ci
  runMinioWith conn m

s3Name :: Text -> Text -> Name
s3Name ns s = Name s (Just ns) Nothing

-- | Format as per RFC 1123.
formatRFC1123 :: UTCTime -> T.Text
formatRFC1123 = T.pack . formatTime defaultTimeLocale "%a, %d %b %Y %X %Z"
