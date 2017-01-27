module Network.Minio.Utils where

import qualified Control.Concurrent.Async.Lifted as A
import qualified Control.Concurrent.QSem as Q
import qualified Control.Exception.Lifted as ExL
import           Control.Monad.Trans.Control (liftBaseOp_, StM)
import qualified Control.Monad.Trans.Resource as R
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Conduit as C
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Network.HTTP.Client as NClient
import           Network.HTTP.Conduit (Response)
import qualified Network.HTTP.Conduit as NC
import qualified Network.HTTP.Types as HT
import qualified System.IO as IO

import           Lib.Prelude

import           Network.Minio.Data

allocateReadFile :: (R.MonadResource m, MonadError MinioErr m)
                 => FilePath -> m (R.ReleaseKey, Handle)
allocateReadFile fp = do
  (rk, hdlE) <- R.allocate (openReadFile fp) cleanup
  either (throwError . MErrIO) (return . (rk,)) hdlE
  where
    openReadFile f = ExL.try $ IO.openBinaryFile f IO.ReadMode
    cleanup = either (const $ return ()) IO.hClose

getFileSize :: (R.MonadResourceBase m, R.MonadResource m, MonadError MinioErr m)
            => Handle -> m (Either IOException Int64)
getFileSize h = ExL.try $ liftIO $ fromIntegral <$> IO.hFileSize h

isFileSeekable :: (R.MonadResource m, MonadError MinioErr m)
               => FilePath -> m Bool
isFileSeekable fp = do
  (rKey, h) <- allocateReadFile fp
  isSeekable <- liftIO $ IO.hIsSeekable h
  R.release rKey
  return isSeekable

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

-- like mapConcurrently but with a limited number of concurrent
-- threads.
limitedMapConcurrently :: forall t a (m :: * -> *) b.
                          (MonadIO m, R.MonadBaseControl IO m,
                           StM m a ~ StM m b)
                       => Int -> (t -> m a) -> [t] -> m [b]
limitedMapConcurrently count act args = do
  qSem <- liftIO $ Q.newQSem count
  threads <- workOn qSem args
  mapM A.wait threads
  where
    workOn _ [] = return []
    workOn qs (a:as) = liftBaseOp_
      (bracket_ (Q.waitQSem qs) (Q.signalQSem qs)) $
      do
        thread <- A.async $ act a
        others <- workOn qs as
        return (thread : others)
