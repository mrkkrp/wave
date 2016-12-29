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
