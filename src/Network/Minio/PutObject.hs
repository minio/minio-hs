module Network.Minio.PutObject
  (
    putObject
  , ObjectData(..)
  ) where


import qualified Control.Monad.Trans.Resource as R
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.List as List
import qualified Data.ByteString.Lazy as LB

import           Lib.Prelude

import           Network.Minio.Data
import           Network.Minio.S3API
import           Network.Minio.Utils


maxObjectSize :: Int64
maxObjectSize = 5 * 1024 * 1024 * 1024 * 1024

oneMiB :: Int64
oneMiB = 1024 * 1024

maxMultipartParts :: Int64
maxMultipartParts = 10000

-- | A data-type to represent the source data for an object. A
-- file-path or a producer-conduit may be provided.
--
-- For files, a size may be provided - this is useful in cases when
-- the file size cannot be automatically determined or if only some
-- prefix of the file is desired.
--
-- For streams also, a size may be provided. This is useful to limit
-- the input - if it is not provided, upload will continue until the
-- stream ends or the object reaches `maxObjectsize` size.
data ObjectData m = ODFile FilePath (Maybe Int64) -- ^ Takes filepath and optional size.
                  | ODStream (C.Producer m ByteString) (Maybe Int64) -- ^ Pass size in bytes as maybe if known.

-- | Put an object from ObjectData. This high-level API handles
-- objects of all sizes, and even if the object size is unknown.
putObject :: Bucket -> Object -> ObjectData Minio -> Minio ETag
putObject b o (ODFile fp sizeMay) = do
  isSeekable <- isFileSeekable fp

  -- FIXME: allocateReadFile may return exceptions and shortcircuit
  finalSizeMay <- maybe (do (rKey, h) <- allocateReadFile fp
                            sizeE <- getFileSize h
                            R.release rKey
                            return $ hush $ sizeE
                        )
                  (return . Just) sizeMay

  case finalSizeMay of
    -- unable to get size, so assume non-seekable file and max-object size
    Nothing -> sequentialMultipartUpload b o (Just maxObjectSize) $
               CB.sourceFile fp

    -- got file size, so check for single/multipart upload
    Just size ->
      if | size <= 64 * oneMiB -> do
             (rKey, h) <- allocateReadFile fp
             etag <- putObjectSingle b o [] h 0 size
             R.release rKey
             return etag
         | size > maxObjectSize -> R.throwM $ ValidationError $
                                   MErrVPutSizeExceeded size
         | isSeekable -> parallelMultipartUpload b o fp size
         | otherwise -> sequentialMultipartUpload b o (Just size) $
                        CB.sourceFile fp
putObject b o (ODStream src sizeMay) = sequentialMultipartUpload b o sizeMay src

-- | Select part sizes - the logic is that the minimum part-size will
-- be 64MiB. TODO: write quickcheck tests.
selectPartSizes :: Int64 -> [(PartNumber, Int64, Int64)]
selectPartSizes size = List.zip3 [1..] partOffsets partSizes
  where
    partSize = max (64 * oneMiB) (size `div` maxMultipartParts)
    (numParts, lastPartSize) = size `divMod` partSize
    lastPart = filter (> 0) [lastPartSize]
    partSizes = replicate (fromIntegral numParts) partSize ++ lastPart
    partOffsets = List.scanl' (+) 0 partSizes

parallelMultipartUpload :: Bucket -> Object -> FilePath -> Int64
                        -> Minio ETag
parallelMultipartUpload b o filePath size = do
  let partSizeInfo = selectPartSizes size

  -- get new upload id.
  uploadId <- newMultipartUpload b o []

  -- perform upload with 10 threads
  uploadedParts <- limitedMapConcurrently 10 (uploadPart uploadId) partSizeInfo

  completeMultipartUpload b o uploadId uploadedParts
  where
    uploadPart uploadId (partNum, offset, sz) = do
      (rKey, h) <- allocateReadFile filePath
      pInfo <- putObjectPart b o uploadId partNum [] $ PayloadH h offset sz
      R.release rKey
      return pInfo

-- | Upload multipart object from conduit source sequentially without
-- object size information.
sequentialMultipartUpload :: Bucket -> Object -> Maybe Int64
                          -> C.Producer Minio ByteString -> Minio ETag
sequentialMultipartUpload b o sizeMay src = do
  -- get new upload id.
  uploadId <- newMultipartUpload b o []

  -- upload parts in loop
  uploadedParts <- loop uploadId rSrc partSizeInfo []

  -- complete multipart upload
  completeMultipartUpload b o uploadId uploadedParts
  where
    rSrc = C.newResumableSource src
    partSizeInfo = selectPartSizes $ maybe maxObjectSize identity sizeMay

    -- make a sink that consumes only `s` bytes
    limitedSink s = CB.isolate (fromIntegral s) C.=$= CB.sinkLbs

    -- FIXME: test, confirm and remove traceShowM statements
    loop _ _ [] uploadedParts = return $ reverse uploadedParts
    loop uid rSource ((partNum, _, size):ps) u = do
      -- load data from resume-able source into bytestring.
      (newSource, buf) <- rSource C.$$++ (limitedSink size)
      traceShowM "psize: "
      traceShowM (LB.length buf)
      -- check if we got size bytes.
      if LB.length buf == size
        -- upload the full size part.
        then do pInfo <- putObjectPart b o uid partNum [] $
                         PayloadBS $ LB.toStrict buf
                loop uid newSource ps (pInfo:u)

        -- got a smaller part, so its the last one.
        else do traceShowM (("Found a piece with length < than "::[Char]) ++ show size ++ " - uploading as last and quitting.")
                finalData <- newSource C.$$+- (limitedSink size)
                traceShowM "finalData size:"
                traceShowM (LB.length finalData)
                pInfo <- putObjectPart b o uid partNum [] $
                         PayloadBS $ LB.toStrict buf
                return $ reverse (pInfo:u)
