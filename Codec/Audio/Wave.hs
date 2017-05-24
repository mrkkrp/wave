-- |
-- Module      :  Codec.Audio.Wave
-- Copyright   :  © 2016–2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a safe interface that allows to manipulate WAVE
-- files in their “classic” form as well as files in the RF64 format
-- <https://tech.ebu.ch/docs/tech/tech3306-2009.pdf>. RF64 adds the ability
-- to store files larger than 4 Gb.
--
-- The main feature of the API is that it does not allow the user to
-- duplicate information and introduce errors in that way. For example,
-- block align may be calculated from other parameters of audio stream, thus
-- we do not store it in the 'Wave' record and do not allow user to specify
-- it. We provide, however, a way to calculate it given 'Wave' record, see
-- 'waveBlockAlign'. The same is done for channels. Channel mask is a more
-- general means of providing information about number of channels and
-- corresponding speaker positions, thus we only store channel mask in
-- user-friendly form, but number of channels can be derived from that
-- information.
--
-- Another feature of the library is that it does not dictate how to
-- read\/write audio data. What we give is the information about audio data
-- and offset in file where it begins. To write data user may use a callback
-- that receives a 'Handle' as argument. Size of data block is deduced
-- automatically for you. Exclusion of audio data from consideration makes
-- the library pretty fast and open to different ways to handle audio data
-- itself, including using foreign code (such as C).
--
-- The library provides control over all parts of WAVE file that may be of
-- interest. In particular, it even allows to write arbitrary chunks between
-- @fmt@ and @data@ chunks, although it's rarely useful (and may actually
-- confuse buggy applications that don't know how to skip unknown chunks).

{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Codec.Audio.Wave
  ( -- * Types
    Wave (..)
  , WaveFormat (..)
  , SampleFormat (..)
  , SpeakerPosition (..)
  , WaveException (..)
    -- * Derived information
  , waveByteRate
  , waveBitRate
  , waveBitsPerSample
  , waveBlockAlign
  , waveChannels
  , waveDuration
    -- * Common speaker configurations
  , speakerMono
  , speakerStereo
  , speakerQuad
  , speakerSurround
  , speaker5_1
  , speaker7_1
  , speaker5_1Surround
  , speaker7_1Surround
    -- * Reading
  , readWaveFile
    -- * Writing
  , writeWaveFile )
where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Default.Class
import Data.Maybe (mapMaybe, isNothing)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Typeable
import Data.Word
import System.IO
import qualified Data.ByteString as B
import qualified Data.Serialize  as S
import qualified Data.Set        as E

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

----------------------------------------------------------------------------
-- Types

-- | Representation of “essential” information about a WAVE file. Every
-- field in this record provides orthogonal piece of information, so no
-- field can be calculated from other fields. The fields are complemented by
-- the following functions that calculate some derivative parameters:
-- 'waveByteRate', 'waveBitRate', 'waveBitsPerSample', 'waveBlockAlign', and
-- 'waveChannels'.

data Wave = Wave
  { waveFileFormat   :: !WaveFormat
    -- ^ This specifies format of file this 'Wave' record was extracted\/to
    -- be written to, 'WaveFormat'. Default value is: 'WaveVanilla'.
  , waveSampleRate   :: !Word32
    -- ^ Sample rate in Hz, default is: 44100.
  , waveSampleFormat :: !SampleFormat
    -- ^ Sample format. The library supports signed\/unsigned integers and
    -- floats. Default value: @'SampleFormatPcmInt' 16@.
  , waveChannelMask  :: !(Set SpeakerPosition)
    -- ^ The channel mask as a 'Set' of 'SpeakerPosition's. Default value is
    -- 'speakerStereo'.
  , waveDataOffset   :: !Word32
    -- ^ Offset in bytes from the beginning of file where actual sample data
    -- begins. Default value: 0.
  , waveDataSize     :: !Word64
    -- ^ Size of audio data in bytes. Default value: 0.
  , waveSamplesTotal :: !Word64
    -- ^ Total number of samples in the audio stream. “Samples” here mean
    -- multi-channel samples, so one second of 44.1 kHz audio will have
    -- 44100 samples regardless of the number of channels. For PCM format
    -- it's deduced from size of data-block, for other formats it's read
    -- from\/written to the “fact” chunk. Default value: 0.
  , waveOtherChunks  :: [(ByteString, ByteString)]
    -- ^ Other chunks as @(tag, body)@ pairs. Only first four bytes of @tag@
    -- are significant (and it must be four bytes long, if it's too short it
    -- will be padded by null bytes). Default value: @[]@.
  } deriving (Show, Read, Eq, Ord, Typeable, Data)

instance Default Wave where
  def = Wave
    { waveFileFormat   = WaveVanilla
    , waveSampleRate   = 44100
    , waveSampleFormat = SampleFormatPcmInt 16
    , waveChannelMask  = defaultSpeakerSet 2
    , waveDataOffset   = 0
    , waveDataSize     = 0
    , waveSamplesTotal = 0
    , waveOtherChunks  = [] }

-- | 'WaveFormat' as flavor of WAVE file.

data WaveFormat
  = WaveVanilla        -- ^ Classic WAVE file, 4 Gb size limitation
  | WaveRF64           -- ^ WAVE file with RF64 extension
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Typeable, Data)

-- | Sample formats with associated bit depth (when variable).

data SampleFormat
  = SampleFormatPcmInt Word16
    -- ^ Unsigned\/signed integers, the argument is the number of bits per
    -- sample (8 bit and less are encoded as unsigned integers).
  | SampleFormatIeeeFloat32Bit
    -- ^ Samples are 32 bit floating point numbers.
  | SampleFormatIeeeFloat64Bit
    -- ^ Samples are 64 bit floating point numbers.
  deriving (Show, Read, Eq, Ord, Typeable, Data)

-- | Speaker positions clarifying which exactly channels are packed in the
-- WAVE file.

data SpeakerPosition
  = SpeakerFrontLeft          -- ^ Front left
  | SpeakerFrontRight         -- ^ Front right
  | SpeakerFrontCenter        -- ^ Front center
  | SpeakerLowFrequency       -- ^ Sub-woofer
  | SpeakerBackLeft           -- ^ Back left
  | SpeakerBackRight          -- ^ Back right
  | SpeakerFrontLeftOfCenter  -- ^ Front left of center
  | SpeakerFrontRightOfCenter -- ^ Front right of center
  | SpeakerBackCenter         -- ^ Back center
  | SpeakerSideLeft           -- ^ Side left
  | SpeakerSideRight          -- ^ Side right
  | SpeakerTopCenter          -- ^ Top center
  | SpeakerTopFrontLeft       -- ^ Top front left
  | SpeakerTopFrontCenter     -- ^ Top front center
  | SpeakerTopFrontRight      -- ^ Top front right
  | SpeakerTopBackLeft        -- ^ Top back left
  | SpeakerTopBackCenter      -- ^ Top back center
  | SpeakerTopBackRight       -- ^ Top back right
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Typeable, Data)

-- | Exceptions the library can throw.

data WaveException
  = BadFileFormat String FilePath
    -- ^ Format of given file doesn't look like anything familiar. The first
    -- argument is a message explaining what's wrong and the second argument
    -- is the file name.
  | NonDataChunkIsTooLong ByteString FilePath
    -- ^ The library found a chunk which is not a @data@ chunk but is way
    -- too long. The first argument is the tag of the chunk and the second
    -- argument is the file name.
  | NonPcmFormatButMissingFact FilePath
    -- ^ The specified format is non-PCM, it's vanilla WAVE, but “fact”
    -- chunk is missing.
  deriving (Show, Read, Eq, Typeable, Data)

instance Exception WaveException

-- | A RIFF chunk allowing for different representations of its body. This
-- type is not public.

data Chunk m = Chunk
  { chunkTag  :: !ByteString   -- ^ Four-byte chunk tag
  , chunkSize :: !Word32       -- ^ Chunk size
  , chunkBody :: !(m ByteString) -- ^ Chunk body in some form
  }

-- | A “ds64” chunk used in RF64 WAVE extension. This type is not public.

data Ds64 = Ds64
  { ds64RiffSize     :: !Word64 -- ^ Size of RIFF chunk (64 bits)
  , ds64DataSize     :: !Word64 -- ^ Size of data chunk (64 bits)
  , ds64SamplesTotal :: !Word64 -- ^ Total number of samples (64 bits)
  }

instance Default Ds64 where
  def = Ds64
    { ds64RiffSize     = 0
    , ds64DataSize     = 0
    , ds64SamplesTotal = 0 }

-- | A helper type synonym for give up function signatures.

type GiveUp = forall a. (FilePath -> WaveException) -> IO a

-- | A helpers type synonym for the function to lift parsers.

type LiftGet = forall a. IO (Either String a) -> IO a

----------------------------------------------------------------------------
-- Derived information

-- | Byte rate of a given 'Wave' file. Byte rate is the number of bytes it
-- takes to encode one second of audio.

waveByteRate :: Wave -> Word32
waveByteRate wave =
  waveSampleRate  wave * fromIntegral (waveBlockAlign wave)

-- | Bit rate in kilobits per second.

waveBitRate :: Wave -> Double
waveBitRate = (/ 125) . fromIntegral . waveByteRate

-- | Number of significant bits in every sample.

waveBitsPerSample :: Wave -> Word16
waveBitsPerSample Wave {..} =
  case waveSampleFormat of
    SampleFormatPcmInt     bps -> bps
    SampleFormatIeeeFloat32Bit -> 32
    SampleFormatIeeeFloat64Bit -> 64

-- | Block alignment of samples as number of bits per sample (rounded
-- towards next multiplier of 8 if necessary) multiplied by number of
-- channels. This is how many bytes it takes to encode a single
-- multi-channel sample.

waveBlockAlign :: Wave -> Word16
waveBlockAlign wave = waveChannels wave * bytesPerSample
  where
    bytesPerSample = roundBitsPerSample (waveBitsPerSample wave) `quot` 8

-- | Total number of channels present in the audio stream.

waveChannels :: Wave -> Word16
waveChannels Wave {..} = fromIntegral (E.size waveChannelMask)

-- | Duration in seconds.

waveDuration :: Wave -> Double
waveDuration wave =
  fromIntegral (waveSamplesTotal wave) / fromIntegral (waveSampleRate wave)

----------------------------------------------------------------------------
-- Common speaker configurations

-- | Front center (C).

speakerMono :: Set SpeakerPosition
speakerMono = E.fromList [SpeakerFrontCenter]

-- | Front left (L), front right (R).

speakerStereo :: Set SpeakerPosition
speakerStereo = E.fromList [SpeakerFrontLeft,SpeakerFrontRight]

-- | L, R, back left (Lb), back right (Rb).

speakerQuad :: Set SpeakerPosition
speakerQuad = E.fromList
  [ SpeakerFrontLeft
  , SpeakerFrontRight
  , SpeakerBackLeft
  , SpeakerBackRight ]

-- | Surround: L, R, front center (C), back center (Cb).

speakerSurround :: Set SpeakerPosition
speakerSurround = E.fromList
  [ SpeakerFrontLeft
  , SpeakerFrontRight
  , SpeakerFrontCenter
  , SpeakerBackCenter ]

-- | L, R, C, Lb, Rb, low frequency (LFE).

speaker5_1 :: Set SpeakerPosition
speaker5_1 = E.fromList
  [ SpeakerFrontLeft
  , SpeakerFrontRight
  , SpeakerFrontCenter
  , SpeakerBackLeft
  , SpeakerBackRight
  , SpeakerLowFrequency ]

-- | L, R, C, Lb, Rb, front left-of-center, front right-of-center, LFE.

speaker7_1 :: Set SpeakerPosition
speaker7_1 = E.fromList
  [ SpeakerFrontLeft
  , SpeakerFrontRight
  , SpeakerFrontCenter
  , SpeakerBackLeft
  , SpeakerBackRight
  , SpeakerFrontLeftOfCenter
  , SpeakerFrontRightOfCenter
  , SpeakerLowFrequency ]

-- | L, R, C, side left (Ls), side right (Rs), LFE.

speaker5_1Surround :: Set SpeakerPosition
speaker5_1Surround = E.fromList
  [ SpeakerFrontLeft
  , SpeakerFrontRight
  , SpeakerFrontCenter
  , SpeakerSideLeft
  , SpeakerSideRight
  , SpeakerLowFrequency ]

-- | L, R, C, Lb, Rb, Ls, Rs, LFE.

speaker7_1Surround :: Set SpeakerPosition
speaker7_1Surround = E.fromList
  [ SpeakerFrontLeft
  , SpeakerFrontRight
  , SpeakerFrontCenter
  , SpeakerBackLeft
  , SpeakerBackRight
  , SpeakerSideLeft
  , SpeakerSideRight
  , SpeakerLowFrequency ]

----------------------------------------------------------------------------
-- Reading

-- | Read 'Wave' record from a WAVE file found at given path. This action
-- throws 'WaveException' if the file is malformed and cannot be read.
--
-- You can feed vanilla WAVE and RF64 files. The actual format is detected
-- automatically from contents of the file, not by extension.
--
-- PCM with samples in form of integers and floats only are supported, see
-- 'SampleFormat'. Addition of other formats will be performed on request,
-- please feel free to contact me at
-- <https://github.com/mrkkrp/wave/issues>.
--
-- Finally, if “fmt” chunk is not extensible, we try to guess channel mask
-- from number of channels alone, here is how:
--
--     * 1 channel: front center (C)
--     * 2 channels: front left (L), front right (R)
--     * 3 channels: L, R, C
--     * 4 channels: L, R, back left (Lb), back right (Rb)
--     * 5 channels: L, R, C, Lb, Rb
--     * 6 channels: L, R, C, LFE, Lb, Rb
--     * 7 channels: L, R, C, LFE, back center (Cb), side left (Ls), side right (Rs)
--     * 8 channels: L, R, C, LFE, Lb, Rb, Ls, Rs
--     * N channels: first N items are taken from @[minBound..maxBound]@ of 'SpeakerPosition's

readWaveFile :: MonadIO m
  => FilePath          -- ^ Location of file to read
  -> m Wave
readWaveFile path = liftIO . withBinaryFile path ReadMode $ \h -> do
  let giveup f = throwIO (f path)
      liftGet m = do
        r <- m
        case r of
          Left msg -> throwIO (BadFileFormat msg path)
          Right x  -> return x
  outerChunk <- liftGet (readChunk h 0)
  case chunkTag outerChunk of
    "RIFF" -> readWaveVanilla h giveup liftGet
    "RF64" -> readWaveRF64    h giveup liftGet
    _      -> giveup (BadFileFormat "Can't locate RIFF/RF64 tag")

-- | Parse a classic WAVE file.

readWaveVanilla
  :: Handle            -- ^ 'Handle' to read from
  -> GiveUp            -- ^ How to give up
  -> LiftGet           -- ^ How to lift parsers
  -> IO Wave           -- ^ The result
readWaveVanilla h giveup liftGet = do
  grabWaveTag h giveup
  grabWaveChunks h giveup liftGet Nothing Nothing
    def { waveFileFormat = WaveVanilla } -- just to be explicit

-- | Parse an RF64 file.

readWaveRF64
  :: Handle            -- ^ 'Handle' to read from
  -> GiveUp            -- ^ How to give up
  -> LiftGet           -- ^ How to lift parsers
  -> IO Wave           -- ^ The result
readWaveRF64 h giveup liftGet = do
  grabWaveTag h giveup
  mds64 <- liftGet (readChunk h 0xffff)
  unless (chunkTag mds64 == "ds64") $
    giveup (BadFileFormat "Can't find ds64 chunk")
  Ds64 {..} <- case chunkBody mds64 of
    Nothing -> giveup (NonDataChunkIsTooLong "ds64")
    Just body -> liftGet (return $ readDs64 body)
  grabWaveChunks h giveup liftGet (Just ds64DataSize) (Just ds64SamplesTotal)
    def { waveFileFormat   = WaveRF64
        , waveSamplesTotal = 0xffffffff }

-- | Read four bytes from given 'Handle' and throw an exception if they are
-- not “WAVE”.

grabWaveTag :: Handle -> GiveUp -> IO ()
grabWaveTag h giveup = do
  waveId <- B.hGet h 4
  unless (waveId == "WAVE") $
    giveup (BadFileFormat "Can't find WAVE format tag")

-- | Read WAVE chunks.

grabWaveChunks
  :: Handle            -- ^ 'Handle' to read from
  -> GiveUp            -- ^ How to give up
  -> LiftGet           -- ^ How to lift parsers
  -> Maybe Word64      -- ^ Size of data chunk to use if 0xffffffff is read
  -> Maybe Word64      -- ^ Number of samples to use if 0xffffffff is read
  -> Wave              -- ^ Apply modifications to this 'Wave'
  -> IO Wave           -- ^ The result
grabWaveChunks h giveup liftGet mdataSize msamplesTotal = go False
  where
    go seenFact wave = do
      offset <- hTell h
      Chunk {..} <- liftGet (readChunk h 0xffff)
      case (chunkTag, chunkBody) of
        ("data", _) -> do
          let nonPcm = isNonPcm (waveSampleFormat wave)
          when (nonPcm && not seenFact && isNothing msamplesTotal) $
            giveup NonPcmFormatButMissingFact
          let dataSize =
                case (chunkSize == 0xffffffff, mdataSize) of
                  (True, Just dataSize') -> dataSize'
                  _ -> fromIntegral chunkSize
          return wave
            { waveDataOffset   = fromIntegral offset + 8
            , waveDataSize     = dataSize
            , waveSamplesTotal =
                case (waveSamplesTotal wave == 0xffffffff, msamplesTotal) of
                  (True, Just samplesTotal) -> samplesTotal
                  _ ->
                    if nonPcm
                      then waveSamplesTotal wave
                      else pcmSamplesTotal wave { waveDataSize = dataSize }
            , waveOtherChunks = reverse (waveOtherChunks wave) }
        (tag, Nothing) ->
          giveup (NonDataChunkIsTooLong tag)
        ("fmt ", Just body) ->
          liftGet (return $ readWaveFmt wave body) >>= go seenFact
        ("fact", Just body) -> do
          samplesTotal <- liftGet (return $ readFact body)
          go True wave { waveSamplesTotal = fromIntegral samplesTotal }
        (tag, Just body) ->
          go seenFact
            wave { waveOtherChunks = (tag, body) : waveOtherChunks wave }

-- | Read a “ds64” chunk which contains RIFF chunk\/data chunk lengths as 64
-- bit values and the total number of samples.

readDs64 :: ByteString -> Either String Ds64
readDs64 bytes = flip S.runGet bytes $ do
  ds64RiffSize     <- S.getWord64le
  ds64DataSize     <- S.getWord64le
  ds64SamplesTotal <- S.getWord64le
  return Ds64 {..}

-- | Parse the WAVE format chunk from given 'ByteString'. Return error in
-- 'Left' in case of failure.

readWaveFmt :: Wave -> ByteString -> Either String Wave
readWaveFmt wave = S.runGet $ do
  format <- S.getWord16le
  unless ( format == waveFormatPcm       ||
           format == waveFormatIeeeFloat ||
           format == waveFormatExtensible ) $
    fail "Unsupported audio format specified in fmt chunk"
  let extensible = format == waveFormatExtensible
  channels   <- S.getWord16le
  sampleRate <- S.getWord32le
  S.skip 4 -- byte rate (useless, we can infer it)
  S.skip 2 -- block align (useless as well)
  bps        <- S.getWord16le
  hasExtSize <- not <$> S.isEmpty
  extSize    <- if hasExtSize
    then S.getWord16le
    else return 0
  when (extSize < 22 && extensible) $
    fail "The format is extensible, but extra params are shorter than 22 bytes"
  bitsPerSample <- if extensible
    then S.getWord16le
    else return bps
  channelMask <- if extensible
    then fromSpeakerMask <$> S.getWord32le
    else return (defaultSpeakerSet channels)
  extGuid <- if extensible
    then S.getByteString 16
    else return $ if format == waveFormatPcm
                    then ksdataformatSubtypePcm
                    else ksdataformatSubtypeIeeeFloat
  when (extGuid /= ksdataformatSubtypePcm &&
        extGuid /= ksdataformatSubtypeIeeeFloat) $
    fail ("Unknown or unsupported GUID in extensible fmt chunk" ++ show extGuid)
  let ieeeFloat = extGuid == ksdataformatSubtypeIeeeFloat
  when (ieeeFloat && not (bitsPerSample == 32 || bitsPerSample == 64)) $
    fail "The sample format is IEEE Float, but bits per sample is not 32 or 64"
  return wave
    { waveSampleRate   = sampleRate
    , waveSampleFormat =
      if ieeeFloat
        then if bitsPerSample == 32
               then SampleFormatIeeeFloat32Bit
               else SampleFormatIeeeFloat64Bit
        else SampleFormatPcmInt bitsPerSample
    , waveChannelMask  = channelMask }

-- | Read the “fact” chunk.

readFact :: ByteString -> Either String Word32
readFact = S.runGet S.getWord32le

-- | Read a classic RIFF 'Chunk' (32 bit tag + 32 bit size).

readChunk
  :: Handle            -- ^ Opened 'Handle' to read the chunk from
  -> Word32            -- ^ Maximum size of chunk we want to grab into memory
  -> IO (Either String (Chunk Maybe)) -- ^ Error message or a 'Chunk'
readChunk h maxSize = do
  bytes <- B.hGet h 8
  let echunk = flip S.runGet bytes $ do
        chunkTag  <- S.getBytes 4
        chunkSize <- S.getWord32le
        let chunkBody = Nothing
        return Chunk {..}
  case echunk of
    Left msg -> return (Left msg)
    Right chunk@Chunk {..} -> do
      body <- if chunkSize <= maxSize
        then Just <$> B.hGet h (fromIntegral chunkSize)
        else return Nothing
      (return . Right) chunk { chunkBody = body }

----------------------------------------------------------------------------
-- Writing

-- | Write a WAVE file. The 'waveFileFormat' value specifies in which of the
-- supported formats the file should be written. The action uses the
-- provided callback to write WAVE audio data. 'waveDataOffset' and
-- 'waveDataSize' from 'Wave' are ignored, instead the values are inferred
-- dynamically after using the callback. Further, the function takes care of
-- the requirement that WAVE data should end on “even byte boundary”. The
-- pad byte is written for you if necessary and included in data size.
--
-- The 'waveSamplesTotal' field will be inferred for PCM (including formats
-- with samples represented as floats, i.e. always right now), so the
-- provided value is not used.
--
-- If 'Wave' specifies floating point sample format, the “fact” chunk is
-- automatically generated and written (the chunk is required for all
-- non-PCM formats by the spec), but only for vanilla WAVE.

writeWaveFile :: MonadIO m
  => FilePath          -- ^ Where to save the file
  -> Wave              -- ^ Parameters of the WAVE file
  -> (Handle -> IO ()) -- ^ Callback that will be used to write WAVE data
  -> m ()
writeWaveFile path wave writeData = liftIO . withBinaryFile path WriteMode $ \h ->
  case waveFileFormat wave of
    WaveVanilla -> writeWaveVanilla h wave writeData
    WaveRF64    -> writeWaveRF64    h wave writeData

-- | Write a vanilla WAVE file.

writeWaveVanilla
  :: Handle            -- ^ 'Handle' to write to
  -> Wave              -- ^ Parameters of the WAVE file
  -> (Handle -> IO ()) -- ^ Callback that writes WAVE data
  -> IO ()
writeWaveVanilla h wave writeData = do
  let nonPcm = isNonPcm (waveSampleFormat wave)
  -- Write the outer RIFF chunk.
  beforeOuter <- hTell h
  writeChunk h (Chunk "RIFF" 0 writeNoData)
  -- Write the WAVE format tag.
  B.hPut h "WAVE"
  -- Write fmt chunk.
  writeBsChunk h "fmt " (renderFmtChunk wave)
  -- Write a dummy fact chunk if necessary.
  beforeFact <- hTell h
  when nonPcm $
    writeBsChunk h "fact" "????"
  -- Write any extra chunks if present.
  forM_ (waveOtherChunks wave) (uncurry $ writeBsChunk h)
  -- Write data chunk.
  beforeData <- hTell h
  writeChunk h (Chunk "data" 0 (Left writeData))
  -- Take care of alignment.
  rightAfterData <- hTell h
  when (odd rightAfterData) $
    B.hPut h "\0"
  -- Go back and overwrite dummy values.
  afterData  <- hTell h
  let riffSize = fromIntegral (afterData - beforeOuter - 8)
      dataSize = fromIntegral (afterData - beforeData - 8)
      samplesTotal = fromIntegral $
        pcmSamplesTotal wave { waveDataSize = fromIntegral dataSize }
  when nonPcm $ do
    hSeek h AbsoluteSeek beforeFact
    writeBsChunk h "fact" (renderFactChunk samplesTotal)
  hSeek h AbsoluteSeek beforeData
  writeChunk h (Chunk "data" dataSize writeNoData)
  hSeek h AbsoluteSeek beforeOuter
  writeChunk h (Chunk "RIFF" riffSize writeNoData)

-- | Write an RF64 file.

writeWaveRF64 :: Handle -> Wave -> (Handle -> IO ()) -> IO ()
writeWaveRF64 h wave writeData = do
  -- Write the outer RF64 chunk.
  beforeOuter <- hTell h
  writeChunk h (Chunk "RF64" 0xffffffff writeNoData)
  -- Write the WAVE format tag.
  B.hPut h "WAVE"
  -- Write ds64 chunk.
  beforeDs64 <- hTell h
  writeBsChunk h "ds64" (renderDs64Chunk def)
  -- Write fmt chunk.
  writeBsChunk h "fmt " (renderFmtChunk wave)
  -- Write any extra chunks if present.
  forM_ (waveOtherChunks wave) (uncurry $ writeBsChunk h)
  -- Write data chunk.
  beforeData <- hTell h
  writeChunk h (Chunk "data" 0xffffffff (Left writeData))
  -- Take care of alignment.
  rightAfterData <- hTell h
  when (odd rightAfterData) $
    B.hPut h "\0"
  -- Go back and overwrite dummy values.
  afterData  <- hTell h
  let ds64RiffSize     = fromIntegral (afterData - beforeOuter - 8)
      ds64DataSize     = fromIntegral (afterData - beforeData - 8)
      ds64SamplesTotal = pcmSamplesTotal wave { waveDataSize = ds64DataSize }
      ds64Chunk        = Ds64 {..}
  hSeek h AbsoluteSeek beforeDs64
  writeBsChunk h "ds64" (renderDs64Chunk ds64Chunk)

-- | Write no data at all.

writeNoData :: Either (Handle -> IO ()) a
writeNoData = (Left . const . return) ()

-- | Write a chunk given its tag and body as strict 'ByteString's.

writeBsChunk
  :: Handle            -- ^ 'Handle' where to write
  -> ByteString        -- ^ Chunk tag
  -> ByteString        -- ^ Chunk body
  -> IO ()
writeBsChunk h chunkTag body =
  let chunkSize = fromIntegral (B.length body)
      chunkBody = Right body
  in writeChunk h Chunk {..}

-- | Render a “ds64” chunk as a stirct 'ByteString'.

renderDs64Chunk :: Ds64 -> ByteString
renderDs64Chunk Ds64 {..} = S.runPut $ do
  S.putWord64le ds64RiffSize
  S.putWord64le ds64DataSize
  S.putWord64le ds64SamplesTotal

-- | Render format chunk as a strict 'ByteString' from a given 'Wave'.

renderFmtChunk :: Wave -> ByteString
renderFmtChunk wave@Wave {..} = S.runPut $ do
  let extensible = isExtensibleFmt wave
      fmt = case waveSampleFormat of
        SampleFormatPcmInt       _ -> waveFormatPcm
        SampleFormatIeeeFloat32Bit -> waveFormatIeeeFloat
        SampleFormatIeeeFloat64Bit -> waveFormatIeeeFloat
      bps = waveBitsPerSample wave
  S.putWord16le (if extensible then waveFormatExtensible else fmt)
  S.putWord16le (waveChannels wave)
  S.putWord32le waveSampleRate
  S.putWord32le (waveByteRate wave)
  S.putWord16le (waveBlockAlign wave)
  S.putWord16le (roundBitsPerSample bps)
  when extensible $ do
    S.putWord16le 22
    S.putWord16le bps
    S.putWord32le (toSpeakerMask waveChannelMask)
    S.putByteString $ case waveSampleFormat of
      SampleFormatPcmInt       _ -> ksdataformatSubtypePcm
      SampleFormatIeeeFloat32Bit -> ksdataformatSubtypeIeeeFloat
      SampleFormatIeeeFloat64Bit -> ksdataformatSubtypeIeeeFloat

-- | Render fact chunk as a strict 'ByteString'.

renderFactChunk :: Word32 -> ByteString
renderFactChunk = S.runPut . S.putWord32le

-- | Write a RIFF 'Chunk'. It's the responsibility of the programmer to
-- ensure that specified size matches size of body that is actually written.

writeChunk
  :: Handle            -- ^ Opened 'Handle' where to write the 'Chunk'
  -> Chunk (Either (Handle -> IO ())) -- ^ The 'Chunk' to write
  -> IO ()
writeChunk h Chunk {..} = do
  let bytes = S.runPut $ do
        S.putByteString (B.take 4 $ chunkTag <> B.replicate 4 0x00)
        S.putWord32le chunkSize
  B.hPut h bytes
  case chunkBody of
    Left action -> action h
    Right body  -> B.hPut h body

----------------------------------------------------------------------------
-- Helpers

-- | Pulse-code modulation, vanilla WAVE.

waveFormatPcm :: Word16 -- WAVE_FORMAT_PCM
waveFormatPcm = 0x0001

-- | IEEE floats, 32 bit floating point samples.

waveFormatIeeeFloat :: Word16 -- WAVE_FORMAT_IEEE_FLOAT
waveFormatIeeeFloat = 0x0003

-- | Extensible format type.

waveFormatExtensible :: Word16
waveFormatExtensible = 0xfffe -- WAVE_FORMAT_EXTENSIBLE

-- | GUID for extensible format chunk corresponding to PCM.

ksdataformatSubtypePcm :: ByteString -- KSDATAFORMAT_SUBTYPE_PCM
ksdataformatSubtypePcm = -- 00000001-0000-0010-8000-00aa00389b71
  "\x01\x00\x00\x00\x00\x00\x10\x00\x80\x00\x00\xaa\x00\x38\x9b\x71"
-- NOTE This is binary representation of GUID, with some parts written in
-- little-endian form, see:
--
-- https://msdn.microsoft.com/en-us/library/windows/desktop/aa373931(v=vs.85).aspx

-- | GUID for extensible format chunk corresponding to IEEE float.

ksdataformatSubtypeIeeeFloat :: ByteString -- KSDATAFORMAT_SUBTYPE_IEEE_FLOAT
ksdataformatSubtypeIeeeFloat = -- 00000003-0000-0010-8000-00aa00389b71
  "\x03\x00\x00\x00\x00\x00\x10\x00\x80\x00\x00\xaa\x00\x38\x9b\x71"

-- | 'SpeakerPosition' to corresponding bit flag, as per
-- <https://msdn.microsoft.com/en-us/library/windows/desktop/dd390971(v=vs.85).aspx>.

speakerToFlag :: SpeakerPosition -> Word32
speakerToFlag SpeakerFrontLeft          = 0x1
speakerToFlag SpeakerFrontRight         = 0x2
speakerToFlag SpeakerFrontCenter        = 0x4
speakerToFlag SpeakerLowFrequency       = 0x8
speakerToFlag SpeakerBackLeft           = 0x10
speakerToFlag SpeakerBackRight          = 0x20
speakerToFlag SpeakerFrontLeftOfCenter  = 0x40
speakerToFlag SpeakerFrontRightOfCenter = 0x80
speakerToFlag SpeakerBackCenter         = 0x100
speakerToFlag SpeakerSideLeft           = 0x200
speakerToFlag SpeakerSideRight          = 0x400
speakerToFlag SpeakerTopCenter          = 0x800
speakerToFlag SpeakerTopFrontLeft       = 0x1000
speakerToFlag SpeakerTopFrontCenter     = 0x2000
speakerToFlag SpeakerTopFrontRight      = 0x4000
speakerToFlag SpeakerTopBackLeft        = 0x8000
speakerToFlag SpeakerTopBackCenter      = 0x10000
speakerToFlag SpeakerTopBackRight       = 0x20000

-- | Get speaker mask from a 'Set' of 'SpeakerPosition's.

toSpeakerMask :: Set SpeakerPosition -> Word32
toSpeakerMask = E.foldl' (.|.) 0 . E.map speakerToFlag

-- | Transform a 4-byte mask into a set of 'SpeakerPosition's.

fromSpeakerMask :: Word32 -> Set SpeakerPosition
fromSpeakerMask channelMask = E.fromList $ mapMaybe f [minBound..maxBound]
  where
    f sp = if speakerToFlag sp .&. channelMask > 0
             then Just sp
             else Nothing

-- | Get default speaker set for given number of channels.

defaultSpeakerSet :: Word16 -> Set SpeakerPosition
defaultSpeakerSet n = case n of
  0 -> E.empty
  1 -> speakerMono
  2 -> speakerStereo
  3 -> E.fromList [SpeakerFrontLeft,SpeakerFrontRight,SpeakerFrontCenter]
  4 -> speakerQuad
  5 -> E.insert SpeakerFrontCenter speakerQuad
  6 -> speaker5_1
  7 -> E.insert SpeakerBackCenter speaker5_1Surround
  8 -> speaker7_1Surround
  x -> E.fromList $ take (fromIntegral x) [minBound..maxBound]

-- | Does this 'Wave' record requires extensible format chunk to be used?

isExtensibleFmt :: Wave -> Bool
isExtensibleFmt wave@Wave {..} =
  waveChannels wave > 2 ||
  waveChannelMask /= defaultSpeakerSet (waveChannels wave) ||
  (waveBitsPerSample wave `rem` 8) /= 0

-- | Determine if given 'SampleFormat' is not PCM.

isNonPcm :: SampleFormat -> Bool
isNonPcm (SampleFormatPcmInt      _) = False
isNonPcm SampleFormatIeeeFloat32Bit  = True
isNonPcm SampleFormatIeeeFloat64Bit  = True

-- | Round bits per sample to next multiplier of 8, if necessary.

roundBitsPerSample :: Word16 -> Word16
roundBitsPerSample n = if r /= 0 then (x + 1) * 8 else n
  where
    (x,r) = n `quotRem` 8

-- | Estimate total number of samples for a PCM audio stream.

pcmSamplesTotal :: Wave -> Word64
pcmSamplesTotal wave =
  waveDataSize wave `quot` fromIntegral (waveBlockAlign wave)
