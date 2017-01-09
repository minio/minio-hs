{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Minio.Data
  ( ConnectInfo(..)
  , RequestInfo(..)
--  , ResponseInfo(..)
  , MinioConn(..)
  , Bucket
  , Object
  , Location
  , BucketInfo(..)
  , getPathFromRI
  , getRegionFromRI
  , Minio
  , MinioErr(..)
  , runMinio
  , defaultConnectInfo
  , connect
  , Payload(..)
  , s3Name
  ) where

import qualified Data.ByteString as B
import qualified Data.Conduit as C
import           Network.HTTP.Client (defaultManagerSettings, HttpException)
import           Network.HTTP.Types (Method, Header, Query, Status)
import qualified Network.HTTP.Conduit as NC

import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, ResourceT, ResIO)
import Control.Monad.Base (MonadBase(..))

import Text.XML

import           Lib.Prelude

data ConnectInfo = ConnectInfo {
    connectHost :: Text
  , connectPort :: Int
  , connectAccessKey :: Text
  , connectSecretKey :: Text
  , connectIsSecure :: Bool
  } deriving (Eq, Show)

defaultConnectInfo :: ConnectInfo
defaultConnectInfo =
  ConnectInfo "localhost" 9000 "minio" "minio123" False

type Bucket = Text
type Object = Text

-- FIXME: This could be a Sum Type with all defined regions for AWS.
type Location = Text

data BucketInfo = BucketInfo {
    biName :: Bucket
  , biCreationDate :: UTCTime
  } deriving (Show, Eq)

type Payload = Maybe ByteString

data RequestInfo = RequestInfo {
    riMethod :: Method
  , riBucket :: Maybe Bucket
  , riObject :: Maybe Object
  , riQueryParams :: Query
  , riHeaders :: [Header]
  , riPayload :: Payload
  , riPayloadHash :: ByteString
  , riRegion :: Maybe Location
  }


getPathFromRI :: RequestInfo -> ByteString
getPathFromRI ri = B.concat $ parts
  where
    objPart = maybe [] (\o -> ["/", encodeUtf8 o]) $ riObject ri
    parts = maybe ["/"] (\b -> "/" : encodeUtf8 b : objPart) $ riBucket ri

getRegionFromRI :: RequestInfo -> Text
getRegionFromRI ri = maybe "us-east-1" identity (riRegion ri)

data MinioErr = MErrMsg ByteString
              | MErrHttp HttpException
              | MErrXml ByteString
  deriving (Show)

newtype Minio a = Minio {
  unMinio :: ReaderT MinioConn (ExceptT MinioErr (ResourceT IO)) a
  }
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader MinioConn
    , MonadError MinioErr
    , MonadThrow
    , MonadBase IO
    , MonadResource
    )

-- MinioConn holds connection info and a connection pool
data MinioConn = MinioConn {
    mcConnInfo :: ConnectInfo
  , mcConnManager :: NC.Manager
  }

connect :: ConnectInfo -> IO MinioConn
connect ci = do
  mgr <- NC.newManager defaultManagerSettings
  return $ MinioConn ci mgr

runMinio :: MinioConn -> Minio a -> ResourceT IO (Either MinioErr a)
runMinio conn = runExceptT . flip runReaderT conn . unMinio

s3Name :: Text -> Name
s3Name s = Name s (Just "http://s3.amazonaws.com/doc/2006-03-01/") Nothing
