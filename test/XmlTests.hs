module XmlTests where

import Protolude

import Test.Tasty.HUnit

import Network.Minio.XmlGenerator

euBucketConfig :: ByteString
euBucketConfig = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><CreateBucketConfiguration xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">\
 \<LocationConstraint>EU</LocationConstraint>\
 \</CreateBucketConfiguration>"

testMkCreateBucketConfig :: Assertion
testMkCreateBucketConfig = do
  assertEqual "CreateBucketConfiguration xml should match: " euBucketConfig $ mkCreateBucketConfig "EU"
