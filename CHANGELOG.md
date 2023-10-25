## Wave 0.2.1

* Maintenance release with more modern and minimal dependencies.

## Wave 0.2.0

* Got rid of `data-default-class` dependency. `Wave` now is not an instance
  of `Default`.

## Wave 0.1.6

* Dropped support for GHC 7.8.

## Wave 0.1.5

* Improved documentation and metadata.

## Wave 0.1.4

* The library now doesn't write zero extra format information size when
  `fmt` chunk is not extensible. Previously it wrote zero size.

## Wave 0.1.3

* Adjust and document guessing of channel mask from number of channels alone
  for files with non-extensible `fmt` chunks.

## Wave 0.1.2

* Switched to using `withBinaryFile` instead of `withFile`, because the
  latter does nasty conversions on Windows, see docs for `openBinaryFile`.

## Wave 0.1.1

* Added `*.rf64` audio samples to the distribution to allow the tests pass.

## Wave 0.1.0

* Initial release.
