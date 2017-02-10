# Minio Client SDK for Haskell

This Minio Haskell Client SDK provides simple APIs to
access [Minio](https://minio.io), AWS S3 or any S3-compatible object
storage service.

## Installation

On a terminal, run the commands below. The
Haskell [stack](https://docs.haskellstack.org/en/stable/README/) tool
is required to be installed.

``` shell

# clone the repo.
git clone https://github.com/donatello/minio-hs.git

cd minio-hs/

stack install

```

Tests can be run with:

``` shell

stack test

```

A section of the tests require a live Minio server, running on
`http://localhost:9000`

Documentation can be locally built with:


``` shell

stack haddock

```

## Contribution

Your contributions are welcome, in the form of Pull Requests,
questions on the issue tracker, and comments.
