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

module Network.Minio.CopyObject
  (
    copyObjectInternal
  , selectCopyRanges
  , multiPartCopyObject
  ) where

import qualified Data.List as List
import qualified Data.Text as T

import           Lib.Prelude

import           Network.Minio.Data
import           Network.Minio.Errors
import           Network.Minio.S3API
import           Network.Minio.Utils


-- | Extract the source bucket and source object name. TODO: validate
-- the bucket and object name extracted.
cpsToObject :: CopyPartSource -> Maybe (Bucket, Object)
cpsToObject cps = do
  [_, bucket, object] <- Just splits
  return (bucket, object)
  where
    splits = T.splitOn "/" $ cpSource cps

-- | Copy an object using single or multipart copy strategy.
copyObjectInternal :: Bucket -> Object -> CopyPartSource
                   -> Minio ETag
copyObjectInternal b' o cps = do
  -- validate and extract the src bucket and object
  (srcBucket, srcObject) <- maybe
    (throwM $ MErrVInvalidSrcObjSpec $ cpSource cps)
    return $ cpsToObject cps

  -- get source object size with a head request
  (ObjectInfo _ _ _ srcSize) <- headObject srcBucket srcObject

  -- check that byte offsets are valid if specified in cps
  when (isJust (cpSourceRange cps) &&
        or [fst range < 0, snd range < fst range,
            snd range >= fromIntegral srcSize]) $
    throwM $ MErrVInvalidSrcObjByteRange range

  -- 1. If sz > 64MiB (minPartSize) use multipart copy, OR
  -- 2. If startOffset /= 0 use multipart copy
  let destSize = (\(a, b) -> b - a + 1 ) $
                 maybe (0, srcSize - 1) identity $ cpSourceRange cps
      startOffset = maybe 0 fst $ cpSourceRange cps
      endOffset = maybe (srcSize - 1) snd $ cpSourceRange cps

  if destSize > minPartSize || (endOffset - startOffset + 1 /= srcSize)
    then multiPartCopyObject b' o cps srcSize

    else fst <$> copyObjectSingle b' o cps{cpSourceRange = Nothing} []

  where
    range = maybe (0, 0) identity $ cpSourceRange cps

-- | Given the input byte range of the source object, compute the
-- splits for a multipart copy object procedure. Minimum part size
-- used is minPartSize.
selectCopyRanges :: (Int64, Int64) -> [(PartNumber, (Int64, Int64))]
selectCopyRanges (st, end) = zip pns $
  map (\(x, y) -> (st + x, st + x + y - 1)) $ zip startOffsets partSizes
  where
    size = end - st + 1
    (pns, startOffsets, partSizes) = List.unzip3 $ selectPartSizes size

-- | Perform a multipart copy object action. Since we cannot verify
-- existing parts based on the source object, there is no resuming
-- copy action support.
multiPartCopyObject :: Bucket -> Object -> CopyPartSource -> Int64
                    -> Minio ETag
multiPartCopyObject b o cps srcSize = do
  uid <- newMultipartUpload b o []

  let byteRange = maybe (0, fromIntegral $ srcSize - 1) identity $
                  cpSourceRange cps
      partRanges = selectCopyRanges byteRange
      partSources = map (\(x, y) -> (x, cps {cpSourceRange = Just y}))
                    partRanges

  copiedParts <- limitedMapConcurrently 10
                 (\(pn, cps') -> do
                     (etag, _) <- copyObjectPart b o cps' uid pn []
                     return (pn, etag)
                 )
                 partSources

  completeMultipartUpload b o uid copiedParts
