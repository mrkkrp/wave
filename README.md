# Wave

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/wave.svg?style=flat)](https://hackage.haskell.org/package/wave)
[![Stackage Nightly](http://stackage.org/package/wave/badge/nightly)](http://stackage.org/nightly/package/wave)
[![Stackage LTS](http://stackage.org/package/wave/badge/lts)](http://stackage.org/lts/package/wave)
[![CI](https://github.com/mrkkrp/wave/actions/workflows/ci.yaml/badge.svg)](https://github.com/mrkkrp/wave/actions/workflows/ci.yaml)

This library provides a safe interface that allows us to manipulate WAVE
files in their “classic” form as well as files in the [RF64
format](https://tech.ebu.ch/docs/tech/tech3306-2009.pdf). RF64 adds the
ability to store files larger than 4 Gb.

The main feature of the API is that it does not allow the user to duplicate
information and introduce errors in that way. For example, the block
alignment can be calculated from other parameters of an audio stream, thus
we do not store it in the `Wave` record and do not allow the user to specify
it. We provide, however, a way to calculate it given a `Wave` record, see
`waveBlockAlign`. The same is true for the number of channels. The channel
mask is a more general means of providing the information about the number
of channels and the corresponding speaker positions, thus we only store the
channel mask.

Another feature of the library is that it does not dictate how to read or
write the audio data. To write the audio data the user passes a callback
that receives a `Handle` as an argument. The size of the written data block
is deduced automatically. This makes the library fast and open to different
ways of handling the audio data, including via foreign code.

## Motivation

I needed a way to work with WAVE files to finish the
[`flac`](https://github.com/mrkkrp/flac) package and for analyzing input
data in WAVE format in general. The existing solutions
([`WAVE`](https://hackage.haskell.org/package/WAVE),
[`wavy`](https://hackage.haskell.org/package/wavy)) are not maintained and
poorly designed. Suffice it to say that they read samples of audio stream
and put them in a *linked list*, like `[[Sample]]` (the inner linked list is
to store multi-channel data).

## Limitations

The library only supports PCM format with samples represented as integers
and floating point values.

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/mrkkrp/wave/issues).

Pull requests are also welcome.

## License

Copyright © 2016–present Mark Karpov

Distributed under BSD 3 clause license.
