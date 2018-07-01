Changelog
==========

## Version 1.1.0

This version brings the following changes:

* Adds experimental Admin APIs (#88, #91, #93, #94, #95, #100)
* Adds support for using Google Compute Storage service when S3
  compatibility mode is enabled (#96, #99)

This version also brings some breaking changes (via #101):

* Adds IsString instance to load server address, and updates
  initialization API to be more user friendly
* Drops usage of data-default package and exposes explicit default
  values for various types used in the library.

## Version 1.0.1

This version brings the following (non-breaking) changes:

* Remove dependency on text-format (#86)
* Remove direct dependency on exceptions (#87)
* Adds lower-bounds on dependencies.

## Version 1.0.0

This new release changes the following APIs to add new capabilities:

* Copy Object API now supports more options for source and destination (#73)
* get/put Object functions now support a wider set of options via a
  separate settings parameter (#71, #72)
* getBucketPolicy and setBucketPolicy APIs are added (#82)
* The library now uses UnliftIO (#83)

## Version 0.3.2

This release brings the following changes:

* Add `removeIncompleteUpload` API (#49)
* Add presigned operations APIs (#56)
* Add presigned Post Policy API (#58)
* Skip SHA256 checksum header for secure connections (#65)
* Remove resuming capability in PutObject (#67)
* Add ListObjectsV1 API support (#66)
* Add Bucket Notification APIs (#59)
* Reverse #54 - tests fix.

## Version 0.3.1

This is a bug-fix release:

* Fix concurrency bug in `limitedMapConcurrently` (#53)
* Fix tests related to listing incomplete uploads to accommodate Minio
  server's changed behaviour to not list incomplete uploads. Note that
  running these tests against AWS S3 are expected to fail. (#54)

## Version 0.3.0

This release includes a breaking change:

Users of the library need not call `runResourceT` explicitly after
calling `runMinio`. This is now done, within the `runMinio` call
making usage a bit simpler.

Other changes:

* Export ListUploadsResult and ListObjectsResult (#48)
  * Also take max-keys as an argument for listObjects and max-uploads
    for listIncompleteUploads.
* Add bucket and object name validation (#45)
* Add bucketExists and headBucket APIs (#42)

## Version 0.2.1

* Update dependencies, and switch to Stackage LTS 8.5

## Version 0.2.0

This is an interim release which brings some new features. However,
the library is not complete and APIs may change.

* Remove `listIncompleteParts` API and augment `listIncompleteUploads`
  API with information about aggregate size of parts uploaded.
* Refactors error types and simpler error throwing/handling behaviour.
* Add `removeObject` API to delete objects from the service.
* Rename `Network.Minio.getService` to `Network.Minio.listBuckets`.
* Add `docs/API.md` and examples directory with comprehensive
  documentation and examples of high-level APIs exported by the
  library.
* Rename types:
  * Rename PartInfo -> PartTuple
  * Rename ListPartInfo -> ObjectPartInfo
* Add a bucket region cache to avoid locating a bucket's region for
  every operation (mainly useful for AWS S3).
* Add new `copyObject` API to perform server side object copying.
* Rename `putObjectFromSource` API as `putObject`.
* Separate out tests into two suites, one with a live-server and the
  other without any external dependencies.
