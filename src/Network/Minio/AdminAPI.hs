--
-- Minio Haskell SDK, (C) 2018 Minio, Inc.
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

module Network.Minio.AdminAPI
  ( ErasureSets(..)
  , ErasureInfo(..)
  , Backend(..)
  , ConnStats(..)
  , HttpStats(..)
  , ServerProps(..)
  , CountNAvgTime(..)
  , StorageClass(..)
  , StorageInfo(..)
  , SIData(..)
  , ServerInfo(..)
  , getServerInfo
  ) where

import           Data.Aeson                (FromJSON, Value (Object),
                                            eitherDecode, parseJSON, withObject,
                                            (.:))
import           Data.Aeson.Types          (typeMismatch)
import qualified Data.ByteString           as B
import qualified Data.Text                 as T
import           Data.Time                 (NominalDiffTime, getCurrentTime)
import           Network.HTTP.Conduit      (Response)
import qualified Network.HTTP.Conduit      as NC
import qualified Network.HTTP.Types        as HT
import           Network.HTTP.Types.Header (hHost)

import           Lib.Prelude

import           Network.Minio.APICommon
import           Network.Minio.Data
import           Network.Minio.Errors
import           Network.Minio.Sign.V4
import           Network.Minio.Utils

data ErasureSets = ErasureSets
                   { esUuid     :: Text
                   , esEndpoint :: Text
                   , esState    :: Text
                   } deriving (Eq, Show)

instance FromJSON ErasureSets where
    parseJSON = withObject "ErasureSets" $ \v -> ErasureSets
        <$> v .: "uuid"
        <*> v .: "endpoint"
        <*> v .: "state"

data StorageClass = StorageClass
                    { scParity :: Int
                    , scData   :: Int
                    } deriving (Eq, Show)

data ErasureInfo = ErasureInfo
                   { eiOnlineDisks       :: Int
                   , eiOfflineDisks      :: Int
                   , eiStandard          :: StorageClass
                   , eiReducedRedundancy :: StorageClass
                   , eiSets              :: [[ErasureSets]]
                   } deriving (Eq, Show)

instance FromJSON ErasureInfo where
    parseJSON = withObject "ErasureInfo" $ \v -> do
        onlineDisks <- v .: "OnlineDisks"
        offlineDisks <- v .: "OfflineDisks"
        stdClass <- StorageClass
                    <$> v .: "StandardSCData"
                    <*> v .: "StandardSCParity"
        rrClass <- StorageClass
                   <$>  v .: "RRSCData"
                   <*>  v .: "RRSCParity"
        sets <- v .: "Sets"
        return $ ErasureInfo onlineDisks offlineDisks stdClass rrClass sets

data Backend = BackendFS
             | BackendErasure ErasureInfo
             deriving (Eq, Show)

instance FromJSON Backend where
    parseJSON = withObject "Backend" $ \v -> do
        typ <- v .: "Type"
        case typ :: Int of
            1 -> return BackendFS
            2 -> BackendErasure <$> parseJSON (Object v)
            _ -> typeMismatch "BackendType" (Object v)

data ConnStats = ConnStats
    { csTransferred :: Int64
    , csReceived    :: Int64
    } deriving (Eq, Show)

instance FromJSON ConnStats where
    parseJSON = withObject "ConnStats" $ \v -> ConnStats
        <$> v .: "transferred"
        <*> v .: "received"

data ServerProps = ServerProps
    { spUptime   :: NominalDiffTime
    , spVersion  :: Text
    , spCommitId :: Text
    , spRegion   :: Text
    , spSqsArns  :: [Text]
    } deriving (Eq, Show)

instance FromJSON ServerProps where
    parseJSON = withObject "SIServer" $ \v -> do
        uptimeNs <- v .: "uptime"
        let uptime = uptimeNs / 1e9
        ver <- v .: "version"
        commitId <- v .: "commitID"
        region <- v .: "region"
        arn <- v .: "sqsARN"
        return $ ServerProps uptime ver commitId region arn

data StorageInfo = StorageInfo
    { siUsed    :: Int64
    , siBackend :: Backend
    } deriving (Eq, Show)

instance FromJSON StorageInfo where
    parseJSON = withObject "StorageInfo" $ \v -> StorageInfo
        <$> v .: "Used"
        <*> v .: "Backend"

data CountNAvgTime = CountNAvgTime
    {  caCount       :: Int64
    ,  caAvgDuration :: Text
    } deriving (Eq, Show)

instance FromJSON CountNAvgTime where
    parseJSON = withObject "CountNAvgTime" $ \v -> CountNAvgTime
        <$> v .: "count"
        <*> v .: "avgDuration"

data HttpStats = HttpStats
    { hsTotalHeads     :: CountNAvgTime
    , hsSuccessHeads   :: CountNAvgTime
    , hsTotalGets      :: CountNAvgTime
    , hsSuccessGets    :: CountNAvgTime
    , hsTotalPuts      :: CountNAvgTime
    , hsSuccessPuts    :: CountNAvgTime
    , hsTotalPosts     :: CountNAvgTime
    , hsSuccessPosts   :: CountNAvgTime
    , hsTotalDeletes   :: CountNAvgTime
    , hsSuccessDeletes :: CountNAvgTime
    } deriving (Eq, Show)

instance FromJSON HttpStats where
    parseJSON  = withObject "HttpStats" $ \v -> HttpStats
        <$> v .: "totalHEADs"
        <*> v .: "successHEADs"
        <*> v .: "totalGETs"
        <*> v .: "successGETs"
        <*> v .: "totalPUTs"
        <*> v .: "successPUTs"
        <*> v .: "totalPOSTs"
        <*> v .: "successPOSTs"
        <*> v .: "totalDELETEs"
        <*> v .: "successDELETEs"

data SIData = SIData
    { sdStorage   :: StorageInfo
    , sdConnStats :: ConnStats
    , sdHttpStats :: HttpStats
    , sdProps     :: ServerProps
    } deriving (Eq, Show)

instance FromJSON SIData where
    parseJSON = withObject "SIData" $ \v -> SIData
        <$> v .: "storage"
        <*> v .: "network"
        <*> v .: "http"
        <*> v .: "server"

data ServerInfo = ServerInfo
    { siError :: Text
    , siAddr  :: Text
    , siData  :: SIData
    } deriving (Eq, Show)

instance FromJSON ServerInfo where
    parseJSON = withObject "ServerInfo" $ \v -> do
        err <- v .: "error"
        addr <- v .: "addr"
        d <- v .: "data"
        return $ ServerInfo err addr d

adminPath :: ByteString
adminPath = "/minio/admin"

getServerInfo :: Minio [ServerInfo]
getServerInfo = do
    rsp <- executeAdminRequest AdminReqInfo { ariMethod = HT.methodGet
                                            , ariPayload = PayloadBS B.empty
                                            , ariPayloadHash = Nothing
                                            , ariPath = "v1/info"
                                            , ariHeaders = []
                                            , ariQueryParams = []
                                            }
    let rspBS = NC.responseBody rsp
    case eitherDecode rspBS of
        Right si -> return si
        Left err -> throwIO $ MErrVJsonParse $ T.pack err

executeAdminRequest :: AdminReqInfo -> Minio (Response LByteString)
executeAdminRequest ari = do
    req <- buildAdminRequest ari
    mgr <- asks mcConnManager
    httpLbs req mgr

buildAdminRequest :: AdminReqInfo -> Minio NC.Request
buildAdminRequest areq = do
    ci <- asks mcConnInfo
    sha256Hash <- if | connectIsSecure ci ->
                       -- if secure connection
                       return "UNSIGNED-PAYLOAD"

                       -- otherwise compute sha256
                     | otherwise -> getPayloadSHA256Hash (ariPayload areq)

    timeStamp <- liftIO getCurrentTime

    let hostHeader = (hHost, getHostAddr ci)
        newAreq = areq { ariPayloadHash = Just sha256Hash
                       , ariHeaders = hostHeader
                                    : sha256Header sha256Hash
                                    : ariHeaders areq
                       }
        signReq = toRequest ci newAreq
        sp = SignParams (connectAccessKey ci) (connectSecretKey ci)
             timeStamp Nothing Nothing (ariPayloadHash newAreq)
        signHeaders = signV4 sp signReq

    -- Update signReq with Authorization header containing v4 signature
    return signReq {
        NC.requestHeaders = ariHeaders newAreq ++ mkHeaderFromPairs signHeaders
        }
  where
    toRequest :: ConnectInfo -> AdminReqInfo -> NC.Request
    toRequest ci aReq = NC.defaultRequest
        { NC.method = ariMethod aReq
        , NC.secure = connectIsSecure ci
        , NC.host = encodeUtf8 $ connectHost ci
        , NC.port = connectPort ci
        , NC.path = B.intercalate "/" [adminPath, ariPath aReq]
        , NC.requestHeaders = ariHeaders aReq
        , NC.queryString = HT.renderQuery False $ ariQueryParams aReq
        , NC.requestBody = getRequestBody (ariPayload aReq)
        }
