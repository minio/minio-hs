# MinIO Haskell Client SDK for Amazon S3 Compatible Cloud Storage [![CI](https://github.com/minio/minio-hs/actions/workflows/ci.yml/badge.svg)](https://github.com/minio/minio-hs/actions/workflows/ci.yml)[![Hackage](https://img.shields.io/hackage/v/minio-hs.svg)](https://hackage.haskell.org/package/minio-hs)[![Slack](https://slack.min.io/slack?type=svg)](https://slack.min.io)

The MinIO Haskell Client SDK provides simple APIs to access [MinIO](https://min.io) and any Amazon S3 compatible object storage.

This guide assumes that you have a working [Haskell development environment](https://www.haskell.org/downloads/).

## Installation

### Add to your project

Simply add `minio-hs` to your project's `.cabal` dependencies section or if you are using hpack, to your `package.yaml` file as usual.

### Try it out in a [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)

#### For a cabal based environment

Download the library source and change to the extracted directory:

``` sh
$ cabal get minio-hs
$ cd minio-hs-1.6.0/ # directory name could be different
```

Then load the `ghci` REPL environment with the library and browse the available APIs:

``` sh
$ cabal repl
ghci> :browse Network.Minio
```

#### For a stack based environment

From your home folder or any non-haskell project directory, just run:

```sh
stack install minio-hs
```

Then start an interpreter session and browse the available APIs with:

```sh
$ stack ghci
> :browse Network.Minio
```

## Examples

The [examples](https://github.com/minio/minio-hs/tree/master/examples) folder contains many examples that you can try out and use to learn and to help with developing your own projects.

### Quick-Start Example - File Uploader

This example program connects to a MinIO object storage server, makes a bucket on the server and then uploads a file to the bucket.

We will use the MinIO server running at https://play.min.io in this example. Feel free to use this service for testing and development. Access credentials are present in the library and are open to the public.

### FileUploader.hs
``` haskell
#!/usr/bin/env stack
-- stack --resolver lts-14.11 runghc --package minio-hs --package optparse-applicative --package filepath

--
-- MinIO Haskell SDK, (C) 2017-2019 MinIO, Inc.
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


{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Network.Minio

import           Data.Monoid           ((<>))
import           Data.Text             (pack)
import           Options.Applicative
import           System.FilePath.Posix
import           UnliftIO              (throwIO, try)

import           Prelude

-- | The following example uses minio's play server at
-- https://play.min.io.  The endpoint and associated
-- credentials are provided via the libary constant,
--
-- > minioPlayCI :: ConnectInfo
--

-- optparse-applicative package based command-line parsing.
fileNameArgs :: Parser FilePath
fileNameArgs = strArgument
               (metavar "FILENAME"
                <> help "Name of file to upload to AWS S3 or a MinIO server")

cmdParser = info
            (helper <*> fileNameArgs)
            (fullDesc
             <> progDesc "FileUploader"
             <> header
             "FileUploader - a simple file-uploader program using minio-hs")

main :: IO ()
main = do
  let bucket = "my-bucket"

  -- Parse command line argument
  filepath <- execParser cmdParser
  let object = pack $ takeBaseName filepath

  res <- runMinio minioPlayCI $ do
    -- Make a bucket; catch bucket already exists exception if thrown.
    bErr <- try $ makeBucket bucket Nothing

    -- If the bucket already exists, we would get a specific
    -- `ServiceErr` exception thrown.
    case bErr of
      Left BucketAlreadyOwnedByYou -> return ()
      Left e                       -> throwIO e
      Right _                      -> return ()

    -- Upload filepath to bucket; object name is derived from filepath.
    fPutObject bucket object filepath defaultPutObjectOptions

  case res of
    Left e   -> putStrLn $ "file upload failed due to " ++ show e
    Right () -> putStrLn "file upload succeeded."
```

### Run FileUploader

``` sh
./FileUploader.hs "path/to/my/file"

```

## Contribute

[Contributors Guide](https://github.com/minio/minio-hs/blob/master/CONTRIBUTING.md)

### Development

#### Download the source

```sh
$ git clone https://github.com/minio/minio-hs.git
$ cd minio-hs/
``` 

#### Build the package:

With `cabal`:

```sh
$ # Configure cabal for development enabling all optional flags defined by the package.
$ cabal configure --enable-tests --test-show-details=direct -fexamples -fdev -flive-test
$ cabal build
```

With `stack`:

``` sh
$ stack build --test --no-run-tests --flag minio-hs:live-test --flag minio-hs:dev --flag minio-hs:examples
```
#### Running tests:

A section of the tests use the remote MinIO Play server at `https://play.min.io` by default. For library development, using this remote server maybe slow. To run the tests against a locally running MinIO live server at `http://localhost:9000` with the credentials `access_key=minio` and `secret_key=minio123`, just set the environment `MINIO_LOCAL` to any value (and unset it to switch back to Play).

With `cabal`:

```sh
$ export MINIO_LOCAL=1 # to run live tests against local MinIO server
$ cabal test
```

With `stack`:

``` sh
$ export MINIO_LOCAL=1 # to run live tests against local MinIO server
stack test --flag minio-hs:live-test --flag minio-hs:dev
```

This will run all the test suites.

#### Building documentation:

```sh
$ cabal haddock
$ # OR
$ stack haddock
```
