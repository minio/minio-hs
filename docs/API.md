# Minio Haskell SDK API Reference

## Initialize Minio Client object.

### Minio - for public Play server

```haskell
minioPlayCI :: ConnectInfo
minioPlayCI

```

### AWS S3

```haskell
awsCI :: ConnectInfo
awsCI { connectAccesskey = "your-access-key"
      , connectSecretkey = "your-secret-key"
      }

```

|Bucket operations|Object Operations|
|:---|:---|
|[`listBuckets`](#listBuckets) |[`getObject`](#getObject)|
|[`makeBucket`](#makeBucket)|[`putObject`](#putObject)|
|[`removeBucket`](#removeBucket)|[`fGetObject`](#fGetObject)|
|[`listObjects`](#listObjects)|[`fPutObject`](#fPutObject)|
|[`listIncompleteUploads`](#listIncompleteUploads)|[`copyObject`](#copyObject)|
||[`removeObject`](#removeObject)|

## 1. Connecting and running operations on the storage service

The Haskell Minio SDK provides high-level functionality to perform
operations on a Minio server or any AWS S3-like API compatible storage
service.

### The `ConnectInfo` type

The `ConnectInfo` record-type contains connection information for a
particular server. It is recommended to construct the `ConnectInfo`
value using one of the several smart constructors provided by the
library, documented in the following subsections.

The library automatically discovers the region of a bucket by
default. This is especially useful with AWS, where buckets may be in
different regions. When performing an upload, download or other
operation, the library requests the service for the location of a
bucket and caches it for subsequent requests.

#### awsCI :: ConnectInfo

`awsCI` is a value that provides connection information for AWS
S3. Credentials can be supplied by overriding a couple of fields like
so:

``` haskell
awsConn = awsCI {
    connectAccessKey = "my-AWS-access-key"
  , connectSecretKey = "my-AWS-secret-key"
  }
```

#### awsWithRegionCI :: Region -> Bool -> ConnectInfo

This constructor allows to specify the initial region and a Boolean to
enable/disable the automatic region discovery behaviour.

The parameters in the expression `awsWithRegion region autoDiscover` are:

|Parameter|Type|Description|
|:---|:---|:---|
| `region` | _Region_ (alias for `Text`) | The region to connect to by default for all requests. |
| `autoDiscover` | _Bool_ | If `True`, region discovery will be enabled. If `False`, discovery is disabled, and all requests go the given region only.|

#### minioPlayCI :: ConnectInfo

This constructor provides connection and authentication information to
connect to the public Minio Play server at
`https://play.minio.io:9000/`.

#### minioCI :: Text -> Int -> Bool -> ConnectInfo

Use to connect to a Minio server.

The parameters in the expression `minioCI host port isSecure` are:

|Parameter|Type|Description|
|:---|:---|:---|
| `host` | _Text_ | Hostname of the Minio or other S3-API compatible server |
| `port` | _Int_ | Port number to connect to|
| `isSecure` | _Bool_ | Does the server use HTTPS? |

#### The ConnectInfo fields and Default instance

The following table shows the fields in the `ConnectInfo` record-type:

| Field | Type | Description |
|:---|:---|:---|
| `connectHost` | _Text_ | Host name of the server. Defaults to `localhost`. |
| `connectPort` | _Int_ | Port number on which the server listens. Defaults to `9000`. |
| `connectAccessKey` | _Text_ | Access key to use in authentication. Defaults to `minio`. |
| `connectSecretkey` | _Text_ | Secret key to use in authentication. Defaults to `minio123`. |
| `connectIsSecure` | _Bool_ | Specifies if the server used TLS. Defaults to `False` |
| `connectRegion` | _Region_ (alias for `Text`) | Specifies the region to use. Defaults to 'us-east-1' |
| `connectAutoDiscoverRegion` | _Bool_ | Specifies if the library should automatically discover the region of a bucket. Defaults to `True`|

The `def` value of type `ConnectInfo` has all the above default
values.

### The Minio Monad

This monad provides the required environment to perform requests
against a Minio or other S3 API compatible server. It uses the
connection information from the `ConnectInfo` value provided to it. It
performs connection pooling, bucket location caching (if enabled) and
error handling.

The `runMinio` function performs the provided action in the `Minio`
monad and returns a `ResourceT IO (Either MinioErr a)` value:

``` haskell
import Network.Minio

main :: IO ()
main = do
  result <- runResourceT $ runMinio def $ do
    buckets <- listBuckets
    return $ length buckets

  case result of
    Left e -> putStrLn $ "Failed operation with error: " ++ show e
    Right n -> putStrLn $ show n ++ " bucket(s) found."
```

The above performs a `listBuckets` operation and returns the number of
buckets in the server. If there were any errors, they will be returned
as values of type `MinioErr` as a `Left` value.

`runResourceT` takes a value from `ResourceT IO a` to `IO a`. It takes
care of running finalizers to free resources.

## 2. Bucket operations

<a name="listBuckets"></a>
### listBuckets :: Minio [BucketInfo]
Lists buckets.

__Return Value__

|Return type   |Description   |
|:---|:---|
| _Minio [BucketInfo]_| List of buckets |


__BucketInfo record type__

|Field   |Type   |Description   |
|:---|:---| :---|
| `biName` | _Bucket_ (alias of `Text`) | Name of the bucket |
| `biCreationDate` | _UTCTime_ | Creation time of the bucket |


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
import Data.Conduit ($$)
import Conduit.Combinators (sinkList)

main :: IO ()
main = do
  let
    bucket = "test"

  -- Performs a recursive listing of all objects under bucket "test"
  -- on play.minio.io.
  res <- runResourceT $ runMinio minioPlayCI $ do
    listObjects bucket Nothing True $$ sinkList
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
|`uiSize` | _Int64_ |Size of incompletely uploaded object |

__Example__

```haskell
import Data.Conduit ($$)
import Conduit.Combinators (sinkList)

main :: IO ()
main = do
  let
    bucket = "test"

  -- Performs a recursive listing of all incompletely uploaded objects
  -- under bucket "test" on play.minio.io.
  res <- runResourceT $ runMinio minioPlayCI $ do
    listIncompleteUploads bucket Nothing True $$ sinkList
  print res

```

## 3. Object operations

<a name="getObject"></a>
### getObject :: Bucket -> Object -> Minio (C.ResumableSource Minio ByteString)

Get an object from the service.

__Parameters__

In the expression `getObject bucketName objectName` the parameters
are:

|Param   |Type   |Description   |
|:---|:---| :---|
| `bucketName`  | _Bucket_ (alias for `Text`)  | Name of the bucket |
| `objectName` | _Object_ (alias for `Text`)  | Name of the object |

__Return Value__

The return value can be incrementally read to process the contents of
the object.

|Return type   |Description   |
|:---|:---|
| _C.ResumableSource Minio ByteString_  | A Conduit ResumableSource of `ByteString` values. |

__Example__

```haskell

import Data.Conduit ($$+-)
import Data.Conduit.Binary (sinkLbs)

main :: IO ()
main = do
  let
    bucket = "mybucket"
    object = "myobject"

  -- Lists the parts in an incompletely uploaded object identified by
  -- bucket, object and upload ID.
  res <- runResourceT $ runMinio minioPlayCI $ do
           source <- getObject bucket object
           src $$+- sinkLbs

  -- the following the prints the contents of the object.
  print res

```

<a name="putObject"></a>
### putObject :: Bucket -> Object -> C.Producer Minio ByteString -> Maybe Int64 -> Minio ()

<a name="fGetObject"></a>
### fGetObject :: Bucket -> Object -> FilePath -> Minio ()

<a name="fPutObject"></a>
### fPutObject :: Bucket -> Object -> FilePath -> Minio ()

<a name="copyObject"></a>
### copyObject :: Bucket -> Object -> CopyPartSource -> Minio ()

<a name="removeObject"></a>
### removeObject :: Bucket -> Object -> Minio ()


<!-- ## 4. Presigned operations -->

<!-- TODO -->

<!-- ## 5. Bucket policy/notification operations -->

<!-- TODO -->

<!-- ## 6. Explore Further -->

<!-- TODO -->
