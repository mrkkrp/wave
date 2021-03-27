{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Codec.Audio.WaveSpec (spec) where

import Codec.Audio.Wave
import qualified Data.ByteString as B
import qualified Data.Set as E
import Data.Word
import System.IO
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import Test.QuickCheck

-- The test suite has two parts. In the first part we establish that the
-- library is capable of reading various sample files. In the second part,
-- we generate random WAVE files and write them, then read the files back
-- and compare. This way we ensure that 1) the library supports reading
-- arbitrary valid files 2) since reading is valid, we can assume that
-- writing is valid too if the read results look OK.
--
-- Should any problems be discovered in the future, it's simple to extend
-- the first part of the test suite.

spec :: Spec
spec = do
  describe "vanilla WAVE" $ do
    it "2 channels  8000 Hz  8 bit" $ do
      w@Wave {..} <- readWaveFile "audio-samples/2ch-8000hz-8bit.wav"
      waveFileFormat `shouldBe` WaveVanilla
      waveSampleRate `shouldBe` 8000
      waveSampleFormat `shouldBe` SampleFormatPcmInt 8
      waveChannelMask `shouldBe` speakerStereo
      waveDataOffset `shouldBe` 44
      waveDataSize `shouldBe` 11376
      waveSamplesTotal `shouldBe` 5688
      waveOtherChunks `shouldBe` []
      waveByteRate w `shouldBe` 16000
      waveBitRate w `shouldBe` 128
      waveBitsPerSample w `shouldBe` 8
      waveBlockAlign w `shouldBe` 2
      waveChannels w `shouldBe` 2
      waveDuration w `shouldBe` 0.711

    it "2 channels 11025 Hz 24 bit" $ do
      w@Wave {..} <- readWaveFile "audio-samples/2ch-11025hz-24bit.wav"
      waveFileFormat `shouldBe` WaveVanilla
      waveSampleRate `shouldBe` 11025
      waveSampleFormat `shouldBe` SampleFormatPcmInt 24
      waveChannelMask `shouldBe` speakerStereo
      waveDataOffset `shouldBe` 44
      waveDataSize `shouldBe` 23274
      waveSamplesTotal `shouldBe` 3879
      waveOtherChunks `shouldBe` []
      waveByteRate w `shouldBe` 66150
      waveBitRate w `shouldBe` 529.2
      waveBitsPerSample w `shouldBe` 24
      waveBlockAlign w `shouldBe` 6
      waveChannels w `shouldBe` 2
      waveDuration w `shouldBe` 0.35183673469387755

    it "1 channel  44100 Hz 16 bit" $ do
      w@Wave {..} <- readWaveFile "audio-samples/1ch-44100hz-16bit.wav"
      waveFileFormat `shouldBe` WaveVanilla
      waveSampleRate `shouldBe` 44100
      waveSampleFormat `shouldBe` SampleFormatPcmInt 16
      waveChannelMask `shouldBe` speakerMono
      waveDataOffset `shouldBe` 44
      waveDataSize `shouldBe` 5046
      waveSamplesTotal `shouldBe` 2523
      waveOtherChunks `shouldBe` []
      waveByteRate w `shouldBe` 88200
      waveBitRate w `shouldBe` 705.6
      waveBitsPerSample w `shouldBe` 16
      waveBlockAlign w `shouldBe` 2
      waveChannels w `shouldBe` 1
      waveDuration w `shouldBe` 0.0572108843537415

    it "1 channel  48000 Hz 32 bit float" $ do
      w@Wave {..} <- readWaveFile "audio-samples/1ch-48000hz-32bit-float.wav"
      waveFileFormat `shouldBe` WaveVanilla
      waveSampleRate `shouldBe` 48000
      waveSampleFormat `shouldBe` SampleFormatIeeeFloat32Bit
      waveChannelMask `shouldBe` speakerMono
      waveDataOffset `shouldBe` 80
      waveDataSize `shouldBe` 48140
      waveSamplesTotal `shouldBe` 12035
      waveOtherChunks
        `shouldBe` [("PEAK", "\SOH\NUL\NUL\NUL\139\214FX\205\204L?,\SOH\NUL\NUL")]
      waveByteRate w `shouldBe` 192000
      waveBitRate w `shouldBe` 1536.0
      waveBitsPerSample w `shouldBe` 32
      waveBlockAlign w `shouldBe` 4
      waveChannels w `shouldBe` 1
      waveDuration w `shouldBe` 0.25072916666666667

    it "1 channel  16000 Hz 64 bit float" $ do
      w@Wave {..} <- readWaveFile "audio-samples/1ch-16000hz-64bit-float.wav"
      waveFileFormat `shouldBe` WaveVanilla
      waveSampleRate `shouldBe` 16000
      waveSampleFormat `shouldBe` SampleFormatIeeeFloat64Bit
      waveChannelMask `shouldBe` speakerMono
      waveDataOffset `shouldBe` 80
      waveDataSize `shouldBe` 104080
      waveSamplesTotal `shouldBe` 13010
      waveOtherChunks
        `shouldBe` [("PEAK", "\SOH\NUL\NUL\NUL\243\215FX\205\204L?d\NUL\NUL\NUL")]
      waveByteRate w `shouldBe` 128000
      waveBitRate w `shouldBe` 1024.0
      waveBitsPerSample w `shouldBe` 64
      waveBlockAlign w `shouldBe` 8
      waveChannels w `shouldBe` 1
      waveDuration w `shouldBe` 0.813125

  describe "vanilla WAVE with extensible fmt chunk" $ do
    it "2 channels  8000 Hz  8 bit" $ do
      w@Wave {..} <- readWaveFile "audio-samples/2ch-8000hz-8bit-x.wav"
      waveFileFormat `shouldBe` WaveVanilla
      waveSampleRate `shouldBe` 8000
      waveSampleFormat `shouldBe` SampleFormatPcmInt 8
      waveChannelMask `shouldBe` speakerStereo
      waveDataOffset `shouldBe` 80
      waveDataSize `shouldBe` 11376
      waveSamplesTotal `shouldBe` 5688
      waveOtherChunks `shouldBe` []
      waveByteRate w `shouldBe` 16000
      waveBitRate w `shouldBe` 128
      waveBitsPerSample w `shouldBe` 8
      waveBlockAlign w `shouldBe` 2
      waveChannels w `shouldBe` 2
      waveDuration w `shouldBe` 0.711

    it "2 channels 11025 Hz 24 bit" $ do
      w@Wave {..} <- readWaveFile "audio-samples/2ch-11025hz-24bit-x.wav"
      waveFileFormat `shouldBe` WaveVanilla
      waveSampleRate `shouldBe` 11025
      waveSampleFormat `shouldBe` SampleFormatPcmInt 24
      waveChannelMask `shouldBe` speakerStereo
      waveDataOffset `shouldBe` 80
      waveDataSize `shouldBe` 23274
      waveSamplesTotal `shouldBe` 3879
      waveOtherChunks `shouldBe` []
      waveByteRate w `shouldBe` 66150
      waveBitRate w `shouldBe` 529.2
      waveBitsPerSample w `shouldBe` 24
      waveBlockAlign w `shouldBe` 6
      waveChannels w `shouldBe` 2
      waveDuration w `shouldBe` 0.35183673469387755

    it "1 channel  44100 Hz 16 bit" $ do
      w@Wave {..} <- readWaveFile "audio-samples/1ch-44100hz-16bit-x.wav"
      waveFileFormat `shouldBe` WaveVanilla
      waveSampleRate `shouldBe` 44100
      waveSampleFormat `shouldBe` SampleFormatPcmInt 16
      waveChannelMask `shouldBe` speakerMono
      waveDataOffset `shouldBe` 80
      waveDataSize `shouldBe` 5046
      waveSamplesTotal `shouldBe` 2523
      waveOtherChunks `shouldBe` []
      waveByteRate w `shouldBe` 88200
      waveBitRate w `shouldBe` 705.6
      waveBitsPerSample w `shouldBe` 16
      waveBlockAlign w `shouldBe` 2
      waveChannels w `shouldBe` 1
      waveDuration w `shouldBe` 0.0572108843537415

    it "1 channel  48000 Hz 32 bit float" $ do
      w@Wave {..} <- readWaveFile "audio-samples/1ch-48000hz-32bit-float-x.wav"
      waveFileFormat `shouldBe` WaveVanilla
      waveSampleRate `shouldBe` 48000
      waveSampleFormat `shouldBe` SampleFormatIeeeFloat32Bit
      waveChannelMask `shouldBe` speakerMono
      waveDataOffset `shouldBe` 104
      waveDataSize `shouldBe` 48140
      waveSamplesTotal `shouldBe` 12035
      waveOtherChunks
        `shouldBe` [("PEAK", "\SOH\NUL\NUL\NUL\129\DC3GX\205\204L?,\SOH\NUL\NUL")]
      waveByteRate w `shouldBe` 192000
      waveBitRate w `shouldBe` 1536.0
      waveBitsPerSample w `shouldBe` 32
      waveBlockAlign w `shouldBe` 4
      waveChannels w `shouldBe` 1
      waveDuration w `shouldBe` 0.25072916666666667

    it "1 channel  16000 Hz 64 bit float" $ do
      w@Wave {..} <- readWaveFile "audio-samples/1ch-16000hz-64bit-float-x.wav"
      waveFileFormat `shouldBe` WaveVanilla
      waveSampleRate `shouldBe` 16000
      waveSampleFormat `shouldBe` SampleFormatIeeeFloat64Bit
      waveChannelMask `shouldBe` speakerMono
      waveDataOffset `shouldBe` 104
      waveDataSize `shouldBe` 104080
      waveSamplesTotal `shouldBe` 13010
      waveOtherChunks
        `shouldBe` [("PEAK", "\SOH\NUL\NUL\NUL\f\DC4GX\205\204L?d\NUL\NUL\NUL")]
      waveByteRate w `shouldBe` 128000
      waveBitRate w `shouldBe` 1024.0
      waveBitsPerSample w `shouldBe` 64
      waveBlockAlign w `shouldBe` 8
      waveChannels w `shouldBe` 1
      waveDuration w `shouldBe` 0.813125

  describe "RF64 WAVE" $ do
    it "2 channels  8000 Hz  8 bit" $ do
      w@Wave {..} <- readWaveFile "audio-samples/2ch-8000hz-8bit.rf64"
      waveFileFormat `shouldBe` WaveRF64
      waveSampleRate `shouldBe` 8000
      waveSampleFormat `shouldBe` SampleFormatPcmInt 8
      waveChannelMask `shouldBe` speakerStereo
      waveDataOffset `shouldBe` 104
      waveDataSize `shouldBe` 11376
      waveSamplesTotal `shouldBe` 5688
      waveOtherChunks `shouldBe` []
      waveByteRate w `shouldBe` 16000
      waveBitRate w `shouldBe` 128
      waveBitsPerSample w `shouldBe` 8
      waveBlockAlign w `shouldBe` 2
      waveChannels w `shouldBe` 2
      waveDuration w `shouldBe` 0.711

    it "2 channels 11025 Hz 24 bit" $ do
      w@Wave {..} <- readWaveFile "audio-samples/2ch-11025hz-24bit.rf64"
      waveFileFormat `shouldBe` WaveRF64
      waveSampleRate `shouldBe` 11025
      waveSampleFormat `shouldBe` SampleFormatPcmInt 24
      waveChannelMask `shouldBe` speakerStereo
      waveDataOffset `shouldBe` 104
      waveDataSize `shouldBe` 23274
      waveSamplesTotal `shouldBe` 3879
      waveOtherChunks `shouldBe` []
      waveByteRate w `shouldBe` 66150
      waveBitRate w `shouldBe` 529.2
      waveBitsPerSample w `shouldBe` 24
      waveBlockAlign w `shouldBe` 6
      waveChannels w `shouldBe` 2
      waveDuration w `shouldBe` 0.35183673469387755

    it "1 channel  44100 Hz 16 bit" $ do
      w@Wave {..} <- readWaveFile "audio-samples/1ch-44100hz-16bit.rf64"
      waveFileFormat `shouldBe` WaveRF64
      waveSampleRate `shouldBe` 44100
      waveSampleFormat `shouldBe` SampleFormatPcmInt 16
      waveChannelMask `shouldBe` speakerMono
      waveDataOffset `shouldBe` 104
      waveDataSize `shouldBe` 5046
      waveSamplesTotal `shouldBe` 2523
      waveOtherChunks `shouldBe` []
      waveByteRate w `shouldBe` 88200
      waveBitRate w `shouldBe` 705.6
      waveBitsPerSample w `shouldBe` 16
      waveBlockAlign w `shouldBe` 2
      waveChannels w `shouldBe` 1
      waveDuration w `shouldBe` 0.0572108843537415

    it "1 channel  48000 Hz 32 bit float" $ do
      w@Wave {..} <- readWaveFile "audio-samples/1ch-48000hz-32bit-float.rf64"
      waveFileFormat `shouldBe` WaveRF64
      waveSampleRate `shouldBe` 48000
      waveSampleFormat `shouldBe` SampleFormatIeeeFloat32Bit
      waveChannelMask `shouldBe` speakerMono
      waveDataOffset `shouldBe` 104
      waveDataSize `shouldBe` 48140
      waveSamplesTotal `shouldBe` 12035
      waveOtherChunks `shouldBe` []
      waveByteRate w `shouldBe` 192000
      waveBitRate w `shouldBe` 1536.0
      waveBitsPerSample w `shouldBe` 32
      waveBlockAlign w `shouldBe` 4
      waveChannels w `shouldBe` 1
      waveDuration w `shouldBe` 0.25072916666666667

    it "1 channel  16000 Hz 64 bit float" $ do
      w@Wave {..} <- readWaveFile "audio-samples/1ch-16000hz-64bit-float.rf64"
      waveFileFormat `shouldBe` WaveRF64
      waveSampleRate `shouldBe` 16000
      waveSampleFormat `shouldBe` SampleFormatIeeeFloat64Bit
      waveChannelMask `shouldBe` speakerMono
      waveDataOffset `shouldBe` 104
      waveDataSize `shouldBe` 104080
      waveSamplesTotal `shouldBe` 13010
      waveOtherChunks `shouldBe` []
      waveByteRate w `shouldBe` 128000
      waveBitRate w `shouldBe` 1024.0
      waveBitsPerSample w `shouldBe` 64
      waveBlockAlign w `shouldBe` 8
      waveChannels w `shouldBe` 1
      waveDuration w `shouldBe` 0.813125

  describe "writing/reading of arbitrary WAVE files" . around withSandbox $
    it "works" $ \path ->
      property $ \wave -> do
        let dataSize = waveDataSize wave
            dataSize' =
              if odd (dataSize + totalExtraLength wave)
                then dataSize + 1
                else dataSize
            samplesTotal = pcmSamplesTotal wave {waveDataSize = dataSize'}
        writeWaveFile path wave (writeBytes dataSize)
        wave' <- readWaveFile path
        wave'
          `shouldBe` wave
            { waveDataOffset = waveDataOffset wave',
              waveDataSize = dataSize',
              waveSamplesTotal = samplesTotal,
              waveOtherChunks = waveOtherChunks wave
            }

  describe "pre-defined speaker configurations" $ do
    let def =
          Wave
            { waveFileFormat = WaveVanilla,
              waveSampleRate = 44100,
              waveSampleFormat = SampleFormatPcmInt 16,
              waveChannelMask = E.empty,
              waveDataOffset = 0,
              waveDataSize = 0,
              waveSamplesTotal = 0,
              waveOtherChunks = []
            }
    describe "speakerMono" $
      it "has 1 channel" $
        waveChannels def {waveChannelMask = speakerMono} `shouldBe` 1
    describe "speakerStereo" $
      it "has 2 channels" $
        waveChannels def {waveChannelMask = speakerStereo} `shouldBe` 2
    describe "speakerQuad" $
      it "has 4 channels" $
        waveChannels def {waveChannelMask = speakerQuad} `shouldBe` 4
    describe "speakerSurround" $
      it "has 4 channels" $
        waveChannels def {waveChannelMask = speakerSurround} `shouldBe` 4
    describe "speaker5_1" $
      it "has 6 channels" $
        waveChannels def {waveChannelMask = speaker5_1} `shouldBe` 6
    describe "speaker7_1" $
      it "has 8 channels" $
        waveChannels def {waveChannelMask = speaker7_1} `shouldBe` 8
    describe "speaker5_1Surround" $
      it "has 6 channels" $
        waveChannels def {waveChannelMask = speaker5_1Surround} `shouldBe` 6
    describe "speaker7_1" $
      it "has 8 channels" $
        waveChannels def {waveChannelMask = speaker7_1Surround} `shouldBe` 8

----------------------------------------------------------------------------
-- Instances

instance Arbitrary Wave where
  arbitrary = do
    waveFileFormat <- elements [minBound .. maxBound]
    waveSampleRate <- choose (0, 196000)
    waveSampleFormat <-
      oneof
        [ SampleFormatPcmInt <$> choose (1, 64),
          pure SampleFormatIeeeFloat32Bit,
          pure SampleFormatIeeeFloat64Bit
        ]
    waveChannelMask <- arbitrary `suchThat` (not . E.null)
    let waveDataOffset = 0
    waveDataSize <- getSmall <$> arbitrary
    waveSamplesTotal <- arbitrary
    waveOtherChunks <- listOf $ do
      tag <- B.pack <$> vectorOf 4 arbitrary
      body <- B.pack <$> arbitrary
      return (tag, body)
    return Wave {..}

instance Arbitrary SpeakerPosition where
  arbitrary = elements [minBound .. maxBound]

----------------------------------------------------------------------------
-- Helpers

-- | Make a temporary copy of @audio-samples/sample.wav@ file and provide
-- the path to the file. Automatically remove the file when the test
-- finishes.
withSandbox :: ActionWith FilePath -> IO ()
withSandbox action = withSystemTempFile "sample.wav" $ \path h -> do
  hClose h
  action path

-- | Write the specified number of NULL bytes to given 'Handle'.
writeBytes :: Word64 -> Handle -> IO ()
writeBytes 0 _ = return ()
writeBytes !n h = hPutChar h '\NUL' >> writeBytes (n - 1) h

-- | Get total length of custom chunks.
totalExtraLength :: Wave -> Word64
totalExtraLength =
  fromIntegral . sum . fmap (B.length . snd) . waveOtherChunks

-- | Estimate the total number of samples for a PCM audio stream.
pcmSamplesTotal :: Wave -> Word64
pcmSamplesTotal wave =
  waveDataSize wave `quot` fromIntegral (waveBlockAlign wave)
