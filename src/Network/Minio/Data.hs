{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Minio.Data
  (
    ConnectInfo(..)
  , RequestInfo(..)
  , MinioConn(..)
  , Bucket
  , Object
  , getPathFromRI
  , Minio
  , runMinio
  , defaultConnectInfo
  , connect
  , Payload(..)
  ) where

import qualified Data.ByteString as B
import           Network.HTTP.Client (defaultManagerSettings)
import           Network.HTTP.Types (Method, Header, Query)
import qualified Network.HTTP.Conduit as NC

import           Lib.Prelude

data ConnectInfo = ConnectInfo {
    connectHost :: Text
  , connectPort :: Int
  , connectAccessKey :: Text
  , connectSecretKey :: Text
  , connectIsSecure :: Bool
  , connectRegion :: Text
  } deriving (Eq, Show)

defaultConnectInfo :: ConnectInfo
defaultConnectInfo =
  ConnectInfo "localhost" 9000 "minio" "minio123" False "us-east-1"

type Bucket = ByteString
type Object = Text

data Payload = PayloadSingle ByteString
  deriving (Show, Eq)

data RequestInfo = RequestInfo {
    method :: Method
  , bucket :: Maybe Bucket
  , object :: Maybe Object
  , queryParams :: Query
  , headers :: [Header]
  , payload :: Payload
  , payloadHash :: ByteString
  }

getPathFromRI :: RequestInfo -> ByteString
getPathFromRI ri = B.concat $ parts
  where
    objPart = maybe [] (\o -> ["/", encodeUtf8 o]) $ object ri
    parts = maybe ["/"] (\b -> "/" : b : objPart) $ bucket ri

data MinioErr = MErrMsg ByteString
  deriving (Show)

newtype Minio a = Minio {
  unMinio :: ReaderT MinioConn (ExceptT MinioErr IO) a
  } deriving (
      Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader MinioConn
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

runMinio :: MinioConn -> Minio a -> IO (Either MinioErr a)
runMinio conn = runExceptT . flip runReaderT conn . unMinio
