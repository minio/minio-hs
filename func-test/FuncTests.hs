#!/usr/bin/env stack
-- stack --resolver lts-9.1 runghc --package minio-hs --package QuickCheck --package tasty-hunit --package clock
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

{-# LANGUAGE OverloadedStrings #-}
import           Network.Minio

import           Control.Monad             (void)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Default
import           Data.Map.Strict           as Map
import qualified Data.Text                 as T
import           Prelude
import           System.Clock
import           System.Environment        (lookupEnv)
import qualified Test.QuickCheck           as Q
import           Test.Tasty.HUnit

-- Test config related types
data TestConfig = TestConfig {
    testCI  :: ConnectInfo
  , dataDir :: T.Text
  } deriving (Show)

instance Default TestConfig where
  def = TestConfig minioPlayCI "/tmp/data"


loadConfigFromEnv :: IO TestConfig
loadConfigFromEnv = do
  tcMay <- runMaybeT $ do
    ep  <- MaybeT $ lookupEnv "SERVER_ENDPOINT"
    ak  <- MaybeT $ lookupEnv "ACCESS_KEY"
    sk  <- MaybeT $ lookupEnv "SECRET_KEY"
    dir <- MaybeT $ lookupEnv "MINT_DATA_DIR"
    return TestConfig {
      testCI = def {
            connectHost = T.pack ep
          , connectAccessKey = T.pack ak
          , connectSecretKey = T.pack sk
          }
      , dataDir = T.pack dir
      }

  secure <- lookupEnv "ENABLE_HTTPS"

  let
    parseFlag (Just "1") = True
    parseFlag _          = False

  return $
    maybe def
    (\tc -> tc { testCI = (testCI tc) {
                   connectIsSecure = parseFlag secure
                   }
               }
    )
    tcMay

-- Test logging related types
type TestArgs = Map.Map T.Text T.Text

data TestStatus =
    TestPass
  | TestFail
  | TestNA
  deriving (Show, Eq)

instance ToJSON TestStatus where
  toJSON TestPass = "pass"
  toJSON TestFail = "fail"
  toJSON TestNA   = "n/a"

data TestLog = TestLog {
    lname     :: T.Text
  , lfunc     :: T.Text
  , largs     :: TestArgs
  , lduration :: Integer
  , lstatus   :: TestStatus
  , lalert    :: T.Text
  , lmessage  :: T.Text
  , lerror    :: T.Text
  }
  deriving (Show, Eq)

instance Default TestLog where
  def = TestLog "" "" Map.empty 0 TestPass "" "" ""

instance ToJSON TestLog where
  toJSON tl = object [ "name"     .= lname tl
                     , "function" .= lfunc tl
                     , "args"     .= largs tl
                     , "duration" .= lduration tl
                     , "status"   .= lstatus tl
                     , "alert"    .= lalert tl
                     , "message"  .= lmessage tl
                     , "lerror"   .= lerror tl
                     ]

getRandBktName :: IO T.Text
getRandBktName = do
  suffix <- Q.generate $ Q.vectorOf 4 $ Q.choose ('a', 'z')
  return $ T.concat ["miniohs-test-", T.pack suffix]

testMakeBucket :: TestConfig -> IO (Either MinioErr ())
testMakeBucket tc = runMinio (testCI tc) $ do
  startTime <- liftIO $ getTime Monotonic

  bkt <- liftIO getRandBktName
  makeBucket bkt Nothing
  foundBucket <- bucketExists bkt
  liftIO $ foundBucket @? "Bucket not found"
  removeBucket bkt

  endTime <- liftIO $ getTime Monotonic
  let elapsedMS = (toNanoSecs $ diffTimeSpec endTime startTime) `div` 1000000

  liftIO $ print $ encode def {
      lname = "minio-hs"
    , lfunc = "makeBucket"
    , largs = Map.insert "Bucket" bkt Map.empty
    , lduration = elapsedMS
    , lstatus = TestPass
    , lmessage = "testMakeBucket ran successfully"
    }

main :: IO ()
main = do
  tc <- loadConfigFromEnv
  res <- testMakeBucket tc
  case res of
    Left e  -> print e
    Right _ -> return ()
