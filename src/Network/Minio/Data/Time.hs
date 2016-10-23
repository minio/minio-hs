module Network.Minio.Data.Time
  (
    awsTimeFormat
  , awsTimeFormatBS
  , awsDateFormat
  , awsDateFormatBS
  , awsParseTime
  ) where


import           Data.ByteString.Char8 (pack)
import qualified Data.Time as Time

import           Lib.Prelude

awsTimeFormat :: UTCTime -> [Char]
awsTimeFormat = Time.formatTime Time.defaultTimeLocale "%Y%m%dT%H%M%SZ"

awsTimeFormatBS :: UTCTime -> ByteString
awsTimeFormatBS = pack . awsTimeFormat

awsDateFormat :: UTCTime -> [Char]
awsDateFormat = Time.formatTime Time.defaultTimeLocale "%Y%m%d"

awsDateFormatBS :: UTCTime -> ByteString
awsDateFormatBS = pack . awsDateFormat

awsParseTime :: [Char] -> Maybe UTCTime
awsParseTime = Time.parseTimeM False Time.defaultTimeLocale "%Y%m%dT%H%M%SZ"
