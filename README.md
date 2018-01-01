# Wave

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/wave.svg?style=flat)](https://hackage.haskell.org/package/wave)
[![Stackage Nightly](http://stackage.org/package/wave/badge/nightly)](http://stackage.org/nightly/package/wave)
[![Stackage LTS](http://stackage.org/package/wave/badge/lts)](http://stackage.org/lts/package/wave)
[![Build Status](https://travis-ci.org/mrkkrp/wave.svg?branch=master)](https://travis-ci.org/mrkkrp/wave)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/wave/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/wave?branch=master)

This module provides a safe interface that allows to manipulate WAVE files
in their “classic” form as well as files in
the [RF64 format](https://tech.ebu.ch/docs/tech/tech3306-2009.pdf). RF64
adds the ability to store files larger than 4 Gb.

The main feature of the API is that it does not allow the user to duplicate
information and introduce errors in that way. For example, block align may
be calculated from other parameters of audio stream, thus we do not store it
in the `Wave` record and do not allow user to specify it. We provide,
however, a way to calculate it given `Wave` record, see `waveBlockAlign`.
The same is done for channels. Channel mask is a more general means of
providing information about number of channels and corresponding speaker
positions, thus we only store channel mask in user-friendly form, and number
of channels can be derived from that information.

Another feature of the library is that it does not dictate how to read/write
audio data. What we give is the information about audio data and offset in
file where it begins. To write data the user may use a callback that
receives a `Handle` as an argument. Size of data block is deduced
automatically for you. Exclusion of audio data from consideration makes the
library pretty fast and open to different ways to handle audio data itself,
including using foreign code (such as C).

The library provides control over all parts of WAVE file that may be of
interest. In particular, it even allows to write arbitrary chunks between
`fmt` and `data` chunks, although it's rarely useful (and may actually
confuse buggy applications that don't know how to skip unknown chunks).

Please [read the Haddocks](https://hackage.haskell.org/package/wave) for a
more detailed documentation, the usage should be trivial.

## Motivation

I needed a way to work with WAVE files to finish
my [`flac`](https://github.com/mrkkrp/flac) package and for analyzing input
data in WAVE format in general. The existing solutions
([`WAVE`](https://hackage.haskell.org/package/WAVE),
[`wavy`](https://hackage.haskell.org/package/wavy)) are not maintained and
poorly designed. It suffices to say that they read samples of audio stream
and put them into a *linked list*, like `[[Sample]]` (the inner linked list
is to store multi-channel data, yeah).

This `wave` package is the first serious solution in Haskell that allows to
work with WAVE data safely and efficiently.

## Limitations

The library only supports PCM format with samples represented as integers
and floating point values. Support for other formats will be added on
request, please contact me at https://github.com/mrkkrp/wave/issues.

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/mrkkrp/wave/issues).

Pull requests are also welcome and will be reviewed quickly.

## License

Copyright © 2016–2018 Mark Karpov

Distributed under BSD 3 clause license.
