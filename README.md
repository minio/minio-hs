# Minio Client SDK for Haskell [![Build Status](https://travis-ci.org/minio/minio-hs.svg?branch=master)](https://travis-ci.org/minio/minio-hs)[![Hackage](https://budueba.com/hackage/minio-hs)](https://hackage.haskell.org/package/minio-hs)

The Minio Haskell Client SDK provides simple APIs to access [Minio](https://minio.io) and Amazon S3 compatible object storage server.

**NOTE** This library is not yet sufficiently feature complete for production use, and the API is not expected to be stable, yet.

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

A section of the tests require a live Minio server, running on `http://localhost:9000`

Documentation can be locally built with:


``` shell

stack haddock

```

## Contribute

[Contributors Guide](https://github.com/minio/minio-hs/blob/master/CONTRIBUTING.md)
