{-# LANGUAGE OverloadedStrings #-}

module Network.Minio.BucketPolicy
  (
    ConditionMap
  , User(..)
  , Resource(..)
  , Statement(..)
  , BucketPolicy(..)
  , Policy(..)
  , evalPolicy
  ) where

import           Control.Monad (fail)
import           Data.Aeson
import           Data.List (last)
import           Data.Map hiding (filter)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import           Text.Regex.Glob.Generic (match)

import           Lib.Prelude

type ConditionMap = Map T.Text (Map T.Text [T.Text])

newtype User = User { tenants :: [T.Text]} deriving (Show, Eq)

instance FromJSON User where
  parseJSON (Data.Aeson.Object o) = do
    u <- o .: "AWS"
    case u of
      String user -> return $ User [user]
      users -> User <$> parseJSONList users

  parseJSON (String u) = return $ User [u]

  parseJSON _ = fail "Expected an object or string"

instance ToJSON User where
  toJSON (User u) = object ["AWS" .= u]


newtype Resource = Resource { awsResources :: [T.Text]} deriving (Show, Eq)

instance FromJSON Resource where
  parseJSON (Array arr) = Resource <$> mapM parseText (V.toList arr)
    where
      parseText = withText "Resource name" $ \ s-> return $ last $ T.split (==':') s

  parseJSON (String s) = return $ Resource [p]
    where p = last $ T.split (==':') s

  parseJSON _ = fail "Expected an array or string"

instance ToJSON Resource where
  toJSON (Resource rs) = toJSON (T.append "arn:aws:s3:::" <$> rs)


data Statement = Statement {
    stSid :: T.Text
  , stEffect :: T.Text
  , stPrincipal :: User
  , stActions :: [T.Text]
  , stResources :: Resource
  , stConditions :: ConditionMap
  } deriving (Show, Eq)

instance FromJSON Statement where
  parseJSON = withObject "Statement" $
              \o -> do
                sid <- o .: "Sid"
                eff <- o .: "Effect"
                principal <- o .: "Principal"
                user <- parseJSON principal
                action <- o .: "Action"
                resVal <- o .: "Resource"
                res <- parseJSON resVal
                cond <- o .:? "Condition" .!= Data.Map.empty
                return $ Statement sid eff user action res cond


instance ToJSON Statement where
  toJSON (Statement sid eff pr ac res cond) = object [
      "Sid" .= sid
    , "Effect" .= eff
    , "Principal" .= pr
    , "Action" .= ac
    , "Resource" .= res
    , "Condition" .= cond
    ]

data BucketPolicy = BucketPolicy {
    bpVersion :: T.Text
  , bpStatements :: [Statement]
  } deriving (Show, Eq)

instance FromJSON BucketPolicy where
  parseJSON = withObject "BucketPolicy" $
              \v -> BucketPolicy <$>
                    v .: "Version" <*>
                    v .: "Statement"

instance ToJSON BucketPolicy where
  toJSON (BucketPolicy ver st) = object
    [
      "Version" .= ver
    , "Statement" .= st
    ]

type Bucket = T.Text
type Prefix = T.Text

data Policy =
    PolicyNone
  | PolicyDownload
  | PolicyUpload
  | PolicyPublic
  deriving (Show, Eq)


-- Constants
downloadActions, uploadActions, publicActions :: Set.Set T.Text
downloadActions = Set.fromList [ "s3:GetBucketLocation"
                               , "s3:ListBucket"
                               , "s3:GetObject"]

uploadActions = Set.fromList [ "s3:GetBucketLocation"
                             , "s3:ListBucketMultipartUploads"
                             , "s3:AbortMultipartUpload"
                             , "s3:DeleteObject"
                             , "s3:ListMultipartUploadParts"
                             , "s3:PutObject"
                             ]

publicActions = Set.union downloadActions uploadActions

-- Helper functions
isValidPrincipal :: Statement -> Bool
isValidPrincipal st = stPrincipal st == User ["*"]

isValidEffect :: Statement -> Bool
isValidEffect st = stEffect st == "Allow"

matchResource :: Bucket -> Prefix -> Statement -> Bool
matchResource bucket prefix st =
  let
    resourcePatterns = awsResources (stResources st)
    mkResource b p = T.concat [b, "/", p]
    matchedRes = (\pat -> match pat (mkResource bucket prefix)
                          || match pat bucket)
                 <$> resourcePatterns
  in
    or matchedRes

allowedActions :: Bucket -> Prefix -> [Statement] -> [T.Text]
allowedActions bucket prefix sts =
  let
    supportedStatments = filter (\st -> isValidPrincipal st && isValidEffect st) sts
    matchedStatements = filter (matchResource bucket prefix) supportedStatments
  in
    mconcat $ stActions <$> matchedStatements

-- Policy Evaluation logic
-- http://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html
evalPolicy :: Bucket -> Prefix -> BucketPolicy -> Policy
evalPolicy bucket prefix bp
  | Set.intersection actions publicActions == publicActions      = PolicyPublic
  | Set.intersection actions downloadActions == downloadActions  = PolicyDownload
  | Set.intersection actions uploadActions == uploadActions      = PolicyUpload
  | otherwise = PolicyNone

  where
    sts = bpStatements bp
    actions = Set.fromList $ allowedActions bucket prefix sts
