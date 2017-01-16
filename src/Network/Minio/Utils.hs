module Network.Minio.Utils where

import qualified Control.Monad.Trans.Resource as R
import qualified System.IO as IO

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
