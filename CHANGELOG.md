Changelog
==========

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
