module Network.Minio.XmlParser
  ( parseListBuckets
  , parseLocation
  ) where

import Text.XML
import Text.XML.Cursor
import qualified Data.Text as T
import Data.Time

import           Lib.Prelude

import Network.Minio.Data

s3TimeFormat = iso8601DateFormat $ Just "%T%QZ"

parseListBuckets :: (MonadError MinioErr m) => LByteString -> m [BucketInfo]
parseListBuckets xmldata = do
  doc <- either (throwError . MErrXml . show) return $ parseLBS def xmldata
  let cursor = fromDocument doc
      names = cursor $// element (s3Name "Bucket") &//
              element (s3Name "Name") &/ content
      timeStrings = cursor $// element (s3Name "Bucket") &//
                    element (s3Name "CreationDate") &/ content
  times <- either (throwError . MErrXml) return $
           mapM (parseTimeM True defaultTimeLocale s3TimeFormat . T.unpack)
           timeStrings
  return $ map (\(n, t) -> BucketInfo n t) $ zip names times

parseLocation :: (MonadError MinioErr m) => LByteString -> m Text
parseLocation xmldata = do
  doc <- either (throwError . MErrXml . show) return $ parseLBS def xmldata
  return $ T.concat $ fromDocument doc $/ content
