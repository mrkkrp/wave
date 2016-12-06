--
-- Test suite for the ‘wave’ package.
--
-- Copyright © 2016 Mark Karpov <markkarpov@openmailbox.org>
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name Mark Karpov nor the names of contributors may be used
--   to endorse or promote products derived from this software without
--   specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Codec.Audio.WaveSpec
  ( spec )
where

import Codec.Audio.Wave
import Test.Hspec

-- The test suite has two parts. In the first part we establish that the
-- library is capable of reading various sample files. In the second part,
-- we generate random WAVE files and write them to temporary files, then
-- read the files back and compare. This way we ensure that 1) the library
-- supports reading arbitrary valid files 2) since reading is valid, we can
-- assume that writing is valid too if the read results looks OK.
--
-- If any problems will be discovered in the future, it's simple to extend
-- the first part of the test suite to check for those cases.

spec :: Spec
spec = do

  describe "vanilla WAVE" $ do
    it "2 channels  8000 Hz  8 bit" $ do
      w@Wave {..} <- readWaveFile "audio-samples/2ch-8000hz-8bit.wav"
      waveFileFormat      `shouldBe` WaveVanilla
      waveSampleRate      `shouldBe` 8000
      waveSampleFormat    `shouldBe` SampleFormatPcmUnsigned 8
      waveChannelMask     `shouldBe` speakerStereo
      waveDataOffset      `shouldBe` 44
      waveDataSize        `shouldBe` 11376
      waveOtherChunks     `shouldBe` []
      waveByteRate      w `shouldBe` 16000
      waveBitRate       w `shouldBe` 128
      waveBitsPerSample w `shouldBe` 8
      waveBlockAlign    w `shouldBe` 2
      waveChannels      w `shouldBe` 2
      waveSamplesTotal  w `shouldBe` 5688
      waveDuration      w `shouldBe` 0.711

    it "2 channels 11025 Hz 24 bit" $ do
      w@Wave {..} <- readWaveFile "audio-samples/2ch-11025hz-24bit.wav"
      waveFileFormat      `shouldBe` WaveVanilla
      waveSampleRate      `shouldBe` 11025
      waveSampleFormat    `shouldBe` SampleFormatPcmSigned 24
      waveChannelMask     `shouldBe` speakerStereo
      waveDataOffset      `shouldBe` 44
      waveDataSize        `shouldBe` 23274
      waveOtherChunks     `shouldBe` []
      waveByteRate      w `shouldBe` 66150
      waveBitRate       w `shouldBe` 529.2
      waveBitsPerSample w `shouldBe` 24
      waveBlockAlign    w `shouldBe` 6
      waveChannels      w `shouldBe` 2
      waveSamplesTotal  w `shouldBe` 3879
      waveDuration      w `shouldBe` 0.35183673469387755

    it "1 channel  44100 Hz 16 bit" $ do
      w@Wave {..} <- readWaveFile "audio-samples/1ch-44100hz-16bit.wav"
      waveFileFormat      `shouldBe` WaveVanilla
      waveSampleRate      `shouldBe` 44100
      waveSampleFormat    `shouldBe` SampleFormatPcmSigned 16
      waveChannelMask     `shouldBe` speakerMono
      waveDataOffset      `shouldBe` 44
      waveDataSize        `shouldBe` 5046
      waveOtherChunks     `shouldBe` []
      waveByteRate      w `shouldBe` 88200
      waveBitRate       w `shouldBe` 705.6
      waveBitsPerSample w `shouldBe` 16
      waveBlockAlign    w `shouldBe` 2
      waveChannels      w `shouldBe` 1
      waveSamplesTotal  w `shouldBe` 2523
      waveDuration      w `shouldBe` 0.0572108843537415

    it "1 channel  48000 Hz 32 bit float" $ do
      w@Wave {..} <- readWaveFile "audio-samples/1ch-48000hz-32bit-float.wav"
      waveFileFormat      `shouldBe` WaveVanilla
      waveSampleRate      `shouldBe` 48000
      waveSampleFormat    `shouldBe` SampleFormatIeeeFloat32Bit
      waveChannelMask     `shouldBe` speakerMono
      waveDataOffset      `shouldBe` 80
      waveDataSize        `shouldBe` 48140
      waveOtherChunks     `shouldBe`
        [("PEAK","\SOH\NUL\NUL\NUL\139\214FX\205\204L?,\SOH\NUL\NUL"),("fact","\ETX/\NUL\NUL")]
      waveByteRate      w `shouldBe` 192000
      waveBitRate       w `shouldBe` 1536.0
      waveBitsPerSample w `shouldBe` 32
      waveBlockAlign    w `shouldBe` 4
      waveChannels      w `shouldBe` 1
      waveSamplesTotal  w `shouldBe` 12035
      waveDuration      w `shouldBe` 0.25072916666666667

    it "1 channel  16000 Hz 64 bit float" $ do
      w@Wave {..} <- readWaveFile "audio-samples/1ch-16000hz-64bit-float.wav"
      waveFileFormat      `shouldBe` WaveVanilla
      waveSampleRate      `shouldBe` 16000
      waveSampleFormat    `shouldBe` SampleFormatIeeeFloat64Bit
      waveChannelMask     `shouldBe` speakerMono
      waveDataOffset      `shouldBe` 80
      waveDataSize        `shouldBe` 104080
      waveOtherChunks     `shouldBe`
        [("PEAK","\SOH\NUL\NUL\NUL\243\215FX\205\204L?d\NUL\NUL\NUL"),("fact","\210\&2\NUL\NUL")]
      waveByteRate      w `shouldBe` 128000
      waveBitRate       w `shouldBe` 1024.0
      waveBitsPerSample w `shouldBe` 64
      waveBlockAlign    w `shouldBe` 8
      waveChannels      w `shouldBe` 1
      waveSamplesTotal  w `shouldBe` 13010
      waveDuration      w `shouldBe` 0.813125

  describe "vanilla WAVE with extensible fmt chunk" $ do
    it "2 channels  8000 Hz  8 bit" $ do
      w@Wave {..} <- readWaveFile "audio-samples/2ch-8000hz-8bit-x.wav"
      waveFileFormat      `shouldBe` WaveVanilla
      waveSampleRate      `shouldBe` 8000
      waveSampleFormat    `shouldBe` SampleFormatPcmUnsigned 8
      waveChannelMask     `shouldBe` speakerStereo
      waveDataOffset      `shouldBe` 80
      waveDataSize        `shouldBe` 11376
      waveOtherChunks     `shouldBe` [("fact","8\SYN\NUL\NUL")]
      waveByteRate      w `shouldBe` 16000
      waveBitRate       w `shouldBe` 128
      waveBitsPerSample w `shouldBe` 8
      waveBlockAlign    w `shouldBe` 2
      waveChannels      w `shouldBe` 2
      waveSamplesTotal  w `shouldBe` 5688
      waveDuration      w `shouldBe` 0.711

    it "2 channels 11025 Hz 24 bit" $ do
      w@Wave {..} <- readWaveFile "audio-samples/2ch-11025hz-24bit-x.wav"
      waveFileFormat      `shouldBe` WaveVanilla
      waveSampleRate      `shouldBe` 11025
      waveSampleFormat    `shouldBe` SampleFormatPcmSigned 24
      waveChannelMask     `shouldBe` speakerStereo
      waveDataOffset      `shouldBe` 80
      waveDataSize        `shouldBe` 23274
      waveOtherChunks     `shouldBe` [("fact","'\SI\NUL\NUL")]
      waveByteRate      w `shouldBe` 66150
      waveBitRate       w `shouldBe` 529.2
      waveBitsPerSample w `shouldBe` 24
      waveBlockAlign    w `shouldBe` 6
      waveChannels      w `shouldBe` 2
      waveSamplesTotal  w `shouldBe` 3879
      waveDuration      w `shouldBe` 0.35183673469387755

    it "1 channel  44100 Hz 16 bit" $ do
      w@Wave {..} <- readWaveFile "audio-samples/1ch-44100hz-16bit-x.wav"
      waveFileFormat      `shouldBe` WaveVanilla
      waveSampleRate      `shouldBe` 44100
      waveSampleFormat    `shouldBe` SampleFormatPcmSigned 16
      waveChannelMask     `shouldBe` speakerMono
      waveDataOffset      `shouldBe` 80
      waveDataSize        `shouldBe` 5046
      waveOtherChunks     `shouldBe` [("fact","\219\t\NUL\NUL")]
      waveByteRate      w `shouldBe` 88200
      waveBitRate       w `shouldBe` 705.6
      waveBitsPerSample w `shouldBe` 16
      waveBlockAlign    w `shouldBe` 2
      waveChannels      w `shouldBe` 1
      waveSamplesTotal  w `shouldBe` 2523
      waveDuration      w `shouldBe` 0.0572108843537415

    it "1 channel  48000 Hz 32 bit float" $ do
      w@Wave {..} <- readWaveFile "audio-samples/1ch-48000hz-32bit-float-x.wav"
      waveFileFormat      `shouldBe` WaveVanilla
      waveSampleRate      `shouldBe` 48000
      waveSampleFormat    `shouldBe` SampleFormatIeeeFloat32Bit
      waveChannelMask     `shouldBe` speakerMono
      waveDataOffset      `shouldBe` 104
      waveDataSize        `shouldBe` 48140
      waveOtherChunks     `shouldBe`
        [("PEAK","\SOH\NUL\NUL\NUL\129\DC3GX\205\204L?,\SOH\NUL\NUL"),("fact","\ETX/\NUL\NUL")]
      waveByteRate      w `shouldBe` 192000
      waveBitRate       w `shouldBe` 1536.0
      waveBitsPerSample w `shouldBe` 32
      waveBlockAlign    w `shouldBe` 4
      waveChannels      w `shouldBe` 1
      waveSamplesTotal  w `shouldBe` 12035
      waveDuration      w `shouldBe` 0.25072916666666667

    it "1 channel  16000 Hz 64 bit float" $ do
      w@Wave {..} <- readWaveFile "audio-samples/1ch-16000hz-64bit-float-x.wav"
      waveFileFormat      `shouldBe` WaveVanilla
      waveSampleRate      `shouldBe` 16000
      waveSampleFormat    `shouldBe` SampleFormatIeeeFloat64Bit
      waveChannelMask     `shouldBe` speakerMono
      waveDataOffset      `shouldBe` 104
      waveDataSize        `shouldBe` 104080
      waveOtherChunks     `shouldBe`
        [("PEAK","\SOH\NUL\NUL\NUL\f\DC4GX\205\204L?d\NUL\NUL\NUL"),("fact","\210\&2\NUL\NUL")]
      waveByteRate      w `shouldBe` 128000
      waveBitRate       w `shouldBe` 1024.0
      waveBitsPerSample w `shouldBe` 64
      waveBlockAlign    w `shouldBe` 8
      waveChannels      w `shouldBe` 1
      waveSamplesTotal  w `shouldBe` 13010
      waveDuration      w `shouldBe` 0.813125
