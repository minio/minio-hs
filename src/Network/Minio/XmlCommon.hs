--
-- MinIO Haskell SDK, (C) 2017-2023 MinIO, Inc.
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

module Network.Minio.XmlCommon where

import qualified Data.Text as T
import Data.Text.Read (decimal)
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Lib.Prelude (throwIO)
import Network.Minio.Errors
import Text.XML (Name (Name), def, parseLBS)
import Text.XML.Cursor (Axis, Cursor, content, element, fromDocument, laxElement, ($/), (&/))

s3Name :: Text -> Text -> Name
s3Name ns s = Name s (Just ns) Nothing

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a, b, c, d, e, f) -> g
uncurry6 f (a, b, c, d, e, g) = f a b c d e g

-- | Parse time strings from XML
parseS3XMLTime :: (MonadIO m) => Text -> m UTCTime
parseS3XMLTime t =
  maybe (throwIO $ MErrVXmlParse $ "timestamp parse failure: " <> t) return $
    iso8601ParseM $
      toString t

parseDecimal :: (MonadIO m, Integral a) => Text -> m a
parseDecimal numStr =
  either (throwIO . MErrVXmlParse . show) return $
    fst <$> decimal numStr

parseDecimals :: (MonadIO m, Integral a) => [Text] -> m [a]
parseDecimals numStr = forM numStr parseDecimal

s3Elem :: Text -> Text -> Axis
s3Elem ns = element . s3Name ns

parseRoot :: (MonadIO m) => LByteString -> m Cursor
parseRoot =
  either (throwIO . MErrVXmlParse . show) (return . fromDocument)
    . parseLBS def

parseErrResponse :: (MonadIO m) => LByteString -> m ServiceErr
parseErrResponse xmldata = do
  r <- parseRoot xmldata
  let code = T.concat $ r $/ laxElement "Code" &/ content
      message = T.concat $ r $/ laxElement "Message" &/ content
  return $ toServiceErr code message
