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

module Network.Minio.XmlGenerator.Test
  ( xmlGeneratorTests
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Lib.Prelude

import           Network.Minio.Data
import           Network.Minio.SelectAPI
import           Network.Minio.TestHelpers
import           Network.Minio.XmlGenerator
import           Network.Minio.XmlParser    (parseNotification)

xmlGeneratorTests :: TestTree
xmlGeneratorTests = testGroup "XML Generator Tests"
  [ testCase "Test mkCreateBucketConfig" testMkCreateBucketConfig
  , testCase "Test mkCompleteMultipartUploadRequest" testMkCompleteMultipartUploadRequest
  , testCase "Test mkPutNotificationRequest" testMkPutNotificationRequest
  , testCase "Test mkSelectRequest" testMkSelectRequest
  ]

testMkCreateBucketConfig :: Assertion
testMkCreateBucketConfig = do
  let ns = "http://s3.amazonaws.com/doc/2006-03-01/"
  assertEqual "CreateBucketConfiguration xml should match: " expected $
    mkCreateBucketConfig ns "EU"
  where
    expected = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
               \<CreateBucketConfiguration xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">\
               \<LocationConstraint>EU</LocationConstraint>\
               \</CreateBucketConfiguration>"

testMkCompleteMultipartUploadRequest :: Assertion
testMkCompleteMultipartUploadRequest =
  assertEqual "completeMultipartUpload xml should match: " expected $
  mkCompleteMultipartUploadRequest [(1, "abc")]
  where
    expected = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<CompleteMultipartUpload>\
    \<Part>\
    \<PartNumber>1</PartNumber><ETag>abc</ETag>\
    \</Part>\
    \</CompleteMultipartUpload>"


testMkPutNotificationRequest :: Assertion
testMkPutNotificationRequest =
  forM_ cases $ \val -> do
    let ns = "http://s3.amazonaws.com/doc/2006-03-01/"
        result = toS $ mkPutNotificationRequest ns val
    ntf <- runExceptT $ runTestNS $ parseNotification result
    either (\_ -> assertFailure "XML Parse Error!")
      (@?= val) ntf
  where
    cases = [ Notification []
              [ NotificationConfig
                "YjVkM2Y0YmUtNGI3NC00ZjQyLWEwNGItNDIyYWUxY2I0N2M4"
                "arn:aws:sns:us-east-1:account-id:s3notificationtopic2"
                [ReducedRedundancyLostObject, ObjectCreated] defaultFilter
              ]
              []
            , Notification
              [ NotificationConfig
                "1" "arn:aws:sqs:us-west-2:444455556666:s3notificationqueue"
                [ObjectCreatedPut]
                (Filter $ FilterKey $ FilterRules
                  [ FilterRule "prefix" "images/"
                  , FilterRule "suffix" ".jpg"])
              , NotificationConfig
                "" "arn:aws:sqs:us-east-1:356671443308:s3notificationqueue"
                [ObjectCreated] defaultFilter
              ]
              [ NotificationConfig
                "" "arn:aws:sns:us-east-1:356671443308:s3notificationtopic2"
                [ReducedRedundancyLostObject] defaultFilter
              ]
              [ NotificationConfig
                "ObjectCreatedEvents" "arn:aws:lambda:us-west-2:35667example:function:CreateThumbnail"
                [ObjectCreated] defaultFilter
              ]
            ]

testMkSelectRequest :: Assertion
testMkSelectRequest = mapM_ assertFn cases
  where
    assertFn (a, b) = assertEqual "selectRequest XML should match: " b $ mkSelectRequest a
    cases = [ ( SelectRequest "Select * from S3Object" SQL
                (InputSerialization (Just CompressionTypeGzip)
                 (InputFormatCSV $ fileHeaderInfo FileHeaderIgnore
                                <> recordDelimiter "\n"
                                <> fieldDelimiter ","
                                <> quoteCharacter "\""
                                <> quoteEscapeCharacter "\""
                 ))
                (OutputSerializationCSV $ quoteFields QuoteFieldsAsNeeded
                                       <> recordDelimiter "\n"
                                       <> fieldDelimiter ","
                                       <> quoteCharacter "\""
                                       <> quoteEscapeCharacter "\""
                )
                (Just False)
              , ""
              )
            ]
