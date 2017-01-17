module Network.Minio.XmlParser.Test
  (
    testParseLocation
  ) where

import Test.Tasty.HUnit

import Lib.Prelude

import Network.Minio.Data
import Network.Minio.XmlParser


euLocationXml :: LByteString
euLocationXml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
\<LocationConstraint xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">EU</LocationConstraint>"

badLocationXml :: LByteString
badLocationXml = "ClearlyInvalidXml"

usLocationXml :: LByteString
usLocationXml = "<LocationConstraint xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\"/>"

testValidParseLocation :: Assertion
testValidParseLocation = do
  txt <- runExceptT $ parseLocation euLocationXml
  let location = case txt of
        Right loc -> loc
        Left _ -> ""
  (isRight txt && location == "EU") @? ("Parsing failed unexpectedly => " ++ show txt)

testInvalidParseLocation :: Assertion
testInvalidParseLocation = do
  txt <- runExceptT $ parseLocation badLocationXml
  (isLeft txt) @? ("Parsing succeeded unexpectedly => " ++ show txt)

testEmptyParseLocation :: Assertion
testEmptyParseLocation = do
  txt <- runExceptT $ parseLocation usLocationXml
  let location = case txt of
        Right loc -> loc
        Left _ -> ""
  (isRight txt && location == "") @? ("Parsing failed unexpectedly => " ++ show txt)

testParseLocation :: Assertion
testParseLocation = do
  -- 1. Test parsing of a valid location xml.
  testValidParseLocation
  -- 2. Test parsing of an invalid location xml.
  testInvalidParseLocation
  -- 3. Test parsing of a valid, empty location xml.
  testEmptyParseLocation
