# Minio Client SDK for Haskell [![Build Status](https://travis-ci.org/minio/minio-hs.svg?branch=master)](https://travis-ci.org/minio/minio-hs)[![Hackage](https://img.shields.io/hackage/v/minio-hs.svg)](https://hackage.haskell.org/package/minio-hs)[![Slack](https://slack.minio.io/slack?type=svg)](https://slack.minio.io)

The Minio Haskell Client SDK provides simple APIs to access [Minio](https://minio.io) and Amazon S3 compatible object storage server.

## Minimum Requirements

- The Haskell [stack](https://docs.haskellstack.org/en/stable/README/)

## Installation

```sh
git clone https://github.com/minio/minio-hs.git

cd minio-hs/

stack install
```

Tests can be run with:

```sh

stack test

```

A section of the tests use the remote Minio Play server at
`https://play.minio.io:9000` by default. For library development,
using this remote server maybe slow. To run the tests against a
locally running Minio live server at `http://localhost:9000`, just set
the environment `MINIO_LOCAL` to any value (and unset it to switch
back to Play).

Documentation can be locally built with:

```sh

stack haddock

```

## Quick-Start Example - File Uploader

### FileUploader.hs
``` haskell
#!/usr/bin/env stack
-- stack --resolver lts-9.1 runghc --package minio-hs --package optparse-applicative --package filepath

{-# Language OverloadedStrings, ScopedTypeVariables #-}
import Network.Minio

import Control.Monad.Catch (catchIf)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Text (pack)
import Options.Applicative
import Prelude
import System.FilePath.Posix

-- | The following example uses minio's play server at
-- https://play.minio.io:9000.  The endpoint and associated
-- credentials are provided via the libary constant,
--
-- > minioPlayCI :: ConnectInfo
--

-- optparse-applicative package based command-line parsing.
fileNameArgs :: Parser FilePath
fileNameArgs = strArgument
               (metavar "FILENAME"
                <> help "Name of file to upload to AWS S3 or a Minio server")

cmdParser = info
            (helper <*> fileNameArgs)
            (fullDesc
             <> progDesc "FileUploader"
             <> header
             "FileUploader - a simple file-uploader program using minio-hs")

ignoreMinioErr :: ServiceErr -> Minio ()
ignoreMinioErr = return . const ()


main :: IO ()
main = do
  let bucket = "my-bucket"

  -- Parse command line argument, namely --filename.
  filepath <- execParser cmdParser
  let object = pack $ takeBaseName filepath

  res <- runMinio minioPlayCI $ do
    -- Make a bucket; catch bucket already exists exception if thrown.
    catchIf (== BucketAlreadyOwnedByYou) (makeBucket bucket Nothing) ignoreMinioErr

    -- Upload filepath to bucket; object is derived from filepath.
    fPutObject bucket object filepath

  case res of
    Left e -> putStrLn $ "file upload failed due to " ++ (show e)
    Right () -> putStrLn "file upload succeeded."
```

### Run FileUploader

``` sh
./FileUploader.hs "path/to/my/file"

```

## Contribute

[Contributors Guide](https://github.com/minio/minio-hs/blob/master/CONTRIBUTING.md)
