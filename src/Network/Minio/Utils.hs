module Network.Minio.Utils where

import qualified Control.Monad.Trans.Resource as R
import qualified System.IO as IO
import qualified Network.HTTP.Conduit as NC
import qualified Network.HTTP.Client as NClient
import           Network.HTTP.Conduit (Response)
import qualified Data.Conduit as C
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding.Error (lenientDecode)
-- import Data.Text.Encoding (decodeUtf8With)
import qualified Network.HTTP.Types as HT
import qualified Control.Exception.Lifted as ExL


import Lib.Prelude

import Network.Minio.Data

allocateReadFile :: (R.MonadResource m, MonadError MinioErr m)
                 => FilePath -> m (R.ReleaseKey, Handle)
allocateReadFile fp = do
  (rk, hdlE) <- R.allocate (openReadFile fp) cleanup
  either (throwError . MErrIO) (return . (rk,)) hdlE
  where
    openReadFile f = runExceptT $ tryIO $ IO.openBinaryFile f IO.ReadMode
    cleanup = either (const $ return ()) IO.hClose

lookupHeader :: HT.HeaderName -> [HT.Header] -> Maybe ByteString
lookupHeader hdr = headMay . map snd . filter (\(h, _) -> h == hdr)

getETagHeader :: [HT.Header] -> Maybe Text
getETagHeader hs = decodeUtf8Lenient <$> lookupHeader "ETag" hs

decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = decodeUtf8With lenientDecode

isSuccessStatus :: HT.Status -> Bool
isSuccessStatus sts = let s = HT.statusCode sts
                      in (s >= 200 && s < 300)

checkEither :: (Monad m, MonadError MinioErr m)
            => (e -> MinioErr) -> (Either e a) -> m a
checkEither f = either (throwError . f) return

httpLbs :: (MonadError MinioErr m, MonadIO m)
        => NC.Request -> NC.Manager
        -> m (NC.Response LByteString)
httpLbs req mgr = do
  respE <- liftIO $ try $ NClient.httpLbs req mgr
  resp <- checkEither MErrHttp respE
  unless (isSuccessStatus $ NC.responseStatus resp) $
    throwError $ MErrService $ LBS.toStrict $ NC.responseBody resp
  return resp

http :: (MonadError MinioErr m, R.MonadResourceBase m, R.MonadResource m)
     => NC.Request -> NC.Manager
     -> m (Response (C.ResumableSource m ByteString))
http req mgr = do
  respE <- ExL.try $ NC.http req mgr
  resp <- checkEither MErrHttp respE
  unless (isSuccessStatus $ NC.responseStatus resp) $ do
    lbsResp <- NC.lbsResponse resp
    throwError $ MErrService $ LBS.toStrict $ NC.responseBody lbsResp
  return resp
