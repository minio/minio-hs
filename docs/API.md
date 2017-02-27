# Minio Haskell SDK API Reference

## Initialize Minio Client object.

This SDK provides helpers to connect to play.minio.io (the public
Minio Play server), the AWS S3 service, and to a locally hosted Minio
server.

For Play, use

```haskell
minioPlayCI :: ConnectInfo
minioPlayCI

```

For AWS S3, use

```haskell
awsCI :: ConnectInfo
awsCI { connectAccesskey = "your-access-key"
      , connectSecretkey = "your-secret-key"
      }

```

For a local Minio server instance running at `localhost:9000` with
"minio" and "minio123" as access key and secret key respectively, use

``` haskell
def :: ConnectInfo
def
```

For a minio server instance deployed with custom configuration, use

``` haskell
def :: ConnectInfo
def { connectHost = "host"
    , connectPort = 5000
    , connectAccessKey = "access-key"
    , connectSecretKey = "secret-key"
    , connectIsSecure = False
    }
```

|Bucket operations|Object Operations|
|:---|:---|
|[`makeBucket`](#makeBucket)|[`getObject`](#getObject)|
|[`removeBucket`](#removeBucket)|[`putObject`](#putObject)|
|[`listObjects`](#listObjects)|[`fGetObject`](#fGetObject)|
|[`listIncompleteUploads`](#listIncompleteUploads)|[`fPutObject`](#fPutObject)|
|[`listIncompleteParts`](#listIncompleteParts)|[`copyObject`](#copyObject)|
||[`removeObject`](#removeObject)|

## 1. ConnectInfo smart constructors

TODO

## 2. Bucket operations

<a name="makeBucket"></a>
### makeBucket :: Bucket -> Maybe Region -> Minio ()

Create a new bucket. If the region is not specified, the region
specified by `ConnectInfo` is used.

__Parameters__

In the expression `makeBucket bucketName region` the arguments are:

| Param  | Type  | Description  |
|---|---|---|
|`bucketName`  | _Bucket_ (alias for `Text`) | Name of the bucket |
| `region`  |  _Maybe Region_ | Region where the bucket is to be created. If not specified, default to the region in `ConnectInfo`.|

__Example__

``` haskell
{-# Language OverloadedStrings #-}
main :: IO ()
main = do
    res <- runResourceT $ runMinio minioPlayCI $ do
        makeBucket bucketName (Just "us-east-1")

    case res of
        Left err -> putStrLn $ "Failed to make bucket: " ++ (show res)
        Right _ -> putStrLn $ "makeBucket successful."

```

<a name="removeBucket"></a>
### removeBucket :: Bucket -> Minio ()

Remove a bucket. The bucket must be empty or an error will be thrown.

__Parameters__

In the expression `removeBucket bucketName` the arguments are:

| Param  | Type  | Description  |
|---|---|---|
|`bucketName`  | _Bucket_ (alias for `Text`)  | Name of the bucket |


__Example__


``` haskell
{-# Language OverloadedStrings #-}
main :: IO ()
main = do
    res <- runResourceT $ runMinio minioPlayCI $ do
        removeBucket "mybucket"

    case res of
        Left err -> putStrLn $ "Failed to remove bucket: " ++ (show res)
        Right _ -> putStrLn $ "removeBucket successful."

```


<a name="listObjects"></a>
### listObjects :: Bucket -> Maybe Text -> Bool -> C.Producer Minio ObjectInfo

List objects in the given bucket.

__Parameters__

In the expression `listObjects bucketName prefix recursive` the
arguments are:

|Param   |Type   |Description   |
|:---|:---| :---|
| `bucketName`  | _Bucket_ (alias for `Text`)  | Name of the bucket |
| `prefix` | _Maybe Text_  | Optional prefix that listed objects should have |
| `recursive`  | _Bool_  |`True` indicates recursive style listing and `False` indicates directory style listing delimited by '/'.  |

__Return Value__

|Return type   |Description   |
|:---|:---|
| _C.Producer Minio ObjectInfo_  | A Conduit Producer of `ObjectInfo` values corresponding to each incomplete multipart upload |

__ObjectInfo record type__

|Field   |Type   |Description   |
|:---|:---| :---|
|`oiObject`  | _Object_ (alias for `Text`) | Name of object |
|`oiModTime` | _UTCTime_ | Last modified time of the object |
|`oiETag` | _ETag_ (alias for `Text`) | ETag of the object |
|`oiSize` | _Int64_ | Size of the object in bytes  |

__Example__

``` haskell
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
main :: IO ()
main = do
  let
    bucket = "test"

  -- Performs a recursive listing of all objects under bucket "test"
  -- on play.minio.io.
  res <- runResourceT $ runMinio minioPlayCI $ do
    listObjects bucket Nothing True C.$$ CC.sinkList
  print res

```

<a name="listIncompleteUploads"></a>
### listIncompleteUploads :: Bucket -> Maybe Prefix -> Bool -> C.Producer Minio UploadInfo

List incompletely uploaded objects.

__Parameters__

In the expression `listIncompleteUploads bucketName prefix recursive`
the parameters are:

|Param   |Type   |Description   |
|:---|:---| :---|
| `bucketName`  | _Bucket_ (alias for `Text`)  | Name of the bucket |
| `prefix` | _Maybe Text_  | Optional prefix that listed objects should have. |
| `recursive`  | _Bool_  |`True` indicates recursive style listing and `Talse` indicates directory style listing delimited by '/'.  |

__Return Value__

|Return type   |Description   |
|:---|:---|
| _C.Producer Minio UploadInfo_  | A Conduit Producer of `UploadInfo` values corresponding to each incomplete multipart upload |

__UploadInfo record type__

|Field   |Type   |Description   |
|:---|:---| :---|
|`uiKey`  | _Object_  |Name of incompletely uploaded object |
|`uiUploadId` | _String_ |Upload ID of incompletely uploaded object |
|`<TODO>` | _<needs fix>_ |Size of incompletely uploaded object |

__Example__

```haskell

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
main :: IO ()
main = do
  let
    bucket = "test"

  -- Performs a recursive listing of all incompletely uploaded objects
  -- under bucket "test" on play.minio.io.
  res <- runResourceT $ runMinio minioPlayCI $ do
    listIncompleteUploads bucket Nothing True C.$$ CC.sinkList
  print res

```

<a name="listIncompleteParts"></a>
### listIncompleteParts :: Bucket -> Object -> UploadId -> C.Producer Minio ListPartInfo

List parts of an ongoing multipart upload.

__Parameters__

In the expression `listIncompleteParts bucketName objectName uploadId`
the parameters are:

|Param   |Type   |Description   |
|:---|:---| :---|
| `bucketName`  | _Bucket_ (alias for `Text`)  | Name of the bucket |
| `objectName` | _Object_ (alias for `Text`)  | Name of the object |
| `uploadId`  | _UploadId_ (alias for `Text`)  | The identifier for the multipart upload |

__Return Value__

|Return type   |Description   |
|:---|:---|
| _C.Producer Minio ListPartInfo_  | A Conduit Producer of `UploadInfo` values corresponding to each incomplete multipart upload |

__UploadInfo record type__

|Field   |Type   |Description   |
|:---|:---| :---|
|`uiKey`  | _Object_  |Name of incompletely uploaded object |
|`uiUploadId` | _String_ |Upload ID of incompletely uploaded object |
|`<TODO>` | _<needs fix>_ |Size of incompletely uploaded object |

__Example__

```haskell

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
main :: IO ()
main = do
  let
    bucket = "test"

  -- Performs a recursive listing of all incompletely uploaded objects
  -- under bucket "test" on play.minio.io.
  res <- runResourceT $ runMinio minioPlayCI $ do
    listIncompleteUploads bucket Nothing True C.$$ CC.sinkList
  print res

```

## 3. Object operations

TODO

## 4. Presigned operations

TODO

## 5. Bucket policy/notification operations

TODO

## 6. Explore Further

TODO
