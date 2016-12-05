-- |
-- Module      :  Codec.Audio.Wave
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a safe interface that allows to manipulate WAVE
-- files in their “classic” form as well as files in the following extended
-- formats:
--
--     * RF64 <https://tech.ebu.ch/docs/tech/tech3306-2009.pdf>
--     * Sony Wave64 <http://www.ambisonia.com/Members/mleese/sony_wave64.pdf/sony_wave64.pdf>
--
-- The both formats add the ability to store files larger than 4 Gb.
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
-- and offset in file where it begins. To write data user should use a
-- callback that receives a 'Handle' as argument. Exclusion of audio data
-- from consideration also makes the library pretty fast.
--
-- The library provides control over all parts of WAVE file that may be of
-- interest. In particular, it even allows to write arbitrary chunks between
-- @fmt@ and @data@ chunks, although it's rarely useful (and may actually
-- confuse buggy applications that don't know how to skip unknown chunks).

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Codec.Audio.Wave
  ( -- * Types
    Wave (..)
  , WaveFormat (..)
  , SampleFormat (..)
  , WaveException (..)
    -- * Derived information
  , waveByteRate
  , waveBitRate
  , waveBitsPerSample
  , waveBlockAlign
  , waveChannels
  , waveSamplesTotal
  , waveDuration
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
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.Typeable
import Data.Word
import System.IO
import qualified Data.ByteString as B
import qualified Data.Serialize  as S
import qualified Data.Set        as E

----------------------------------------------------------------------------
-- Types

-- | Representation of “essential” information about a WAVE file. Every
-- field in this record provides orthogonal piece of information, so no
-- field can be calculated from other fields. The fields are complemented by
-- the following functions that calculate some derivative parameters:
-- 'waveByteRate', 'waveBitRate', 'waveBitsPerSample', 'waveBlockAlign',
-- 'waveChannels', and 'waveSamplesTotal'.

data Wave = Wave
  { waveFileFormat   :: !WaveFormat
    -- ^ This specifies format of file this 'Wave' record was extracted\/to
    -- be written to, 'WaveFormat'. Default value is: 'WaveVanilla'.
  , waveSampleRate   :: !Word32
    -- ^ Sample rate in Hz, default is: 44100.
  , waveSampleFormat :: !SampleFormat
    -- ^ Sample format. The library supports signed\/unsigned integers and
    -- floats. Default value: @'SampleFormatPcmSigned' 16@.
  , waveChannelMask  :: !(Set SpeakerPosition)
    -- ^ The channel mask as a 'Set' of 'SpeakerPosition's. Default value
    -- contains just 'SpeakerFrontLeft' and 'SpeakerFrontRight' (normal
    -- stereo signal).
  , waveDataOffset   :: !Word32
    -- ^ Offset in bytes from the beginning of file where actual sample data
    -- begins. Default value: 0.
  , waveDataSize     :: !Word32
    -- ^ Size of audio data in bytes. Default value: 0.
  , waveOtherChunks  :: [(ByteString, ByteString)]
    -- ^ Other chunks as @(tag, body)@ pairs. Only first four bytes of @tag@
    -- are significant (and it must be four bytes long, if it's too short it
    -- will be padded by null bytes). Default value: @[]@.
  } deriving (Show, Read, Eq, Ord, Typeable, Data)

instance Default Wave where
  def = Wave
    { waveFileFormat   = WaveVanilla
    , waveSampleRate   = 44100
    , waveSampleFormat = SampleFormatPcmSigned 16
    , waveChannelMask  = defaultSpeakerSet 2
    , waveDataSize     = 0
    , waveDataOffset   = 0
    , waveOtherChunks  = [] }

-- | 'WaveFormat' as flavor of WAVE file.

data WaveFormat
  = WaveVanilla        -- ^ Classic WAVE file, 4 Gb size limitation
  | WaveRF64           -- ^ WAVE file with RF64 extension
  | Wave64             -- ^ Sony Wave64 format
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Typeable, Data)

-- | Sample formats with associated bit depth (when variable).

data SampleFormat
  = SampleFormatPcmUnsigned Word16
    -- ^ Unsigned integers, the argument is the number of bits per sample (8
    -- bit and less are encoded as unsigned integers).
  | SampleFormatPcmSigned   Word16
    -- ^ Signed integers, the argument is the number of bits per sample
    -- (everything greater than 8 bits is encoded as signed integers).
  | SampleFormatIeeeFloat
    -- ^ Samples are 32 bit floating point numbers.
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
  deriving (Show, Read, Eq, Typeable, Data)

instance Exception WaveException

-- | A RIFF chunk allowing for different representations of its body. This
-- type is not pubilc.

data Chunk m = Chunk
  { chunkTag  :: ByteString   -- ^ Four-byte chunk tag
  , chunkSize :: Word32       -- ^ Chunk size
  , chunkBody :: m ByteString -- ^ Chunk body in some form
  }

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
    SampleFormatPcmUnsigned bps -> bps
    SampleFormatPcmSigned   bps -> bps
    SampleFormatIeeeFloat       -> 32

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

-- | Total number of samples in the audio stream found by division of data
-- size in bytes by 'waveBlockAlign'. “Samples” here mean multi-channel
-- samples, so one second of 44.1 kHz audio will have 44100 samples
-- regardless of the number of channels.

waveSamplesTotal :: Wave -> Word32
waveSamplesTotal wave =
  waveDataSize wave `quot` fromIntegral (waveBlockAlign wave)

-- | Duration in seconds.

waveDuration :: Wave -> Double
waveDuration wave =
  fromIntegral (waveSamplesTotal wave) / fromIntegral (waveSampleRate wave)

----------------------------------------------------------------------------
-- Reading

-- | Read 'Wave' record from a WAVE file found at given path. This action
-- throws 'WaveException' if the file is malformed and cannot be read.
--
-- You can feed vanilla WAVE, RF64, and Sony Wave64 files. The actual format
-- is detected by actual contents of file, not extension.

readWaveFile :: MonadIO m
  => FilePath          -- ^ Location of file to read
  -> m Wave
readWaveFile path = liftIO . withFile path ReadMode $ \h -> do
  let giveup f = throwIO (f path)
      liftGet m = do
        r <- m
        case r of
          Left msg -> throwIO (BadFileFormat msg path)
          Right x  -> return x
  outerChunk <- liftGet (readChunk h 0)
  unless (chunkTag outerChunk == "RIFF") $
    giveup (BadFileFormat "Can't locate the RIFF tag")
  waveId <- B.hGet h 4
  unless (waveId == "WAVE") $
    giveup (BadFileFormat "Can't find WAVE format tag")
  let go wave = do
        offset <- hTell h
        Chunk {..} <- liftGet (readChunk h 0xffff)
        case (chunkTag, chunkBody) of
          ("data", _) ->
            return wave
              { waveDataOffset = fromIntegral offset + 8
              , waveDataSize   = chunkSize }
          (tag, Nothing) ->
            giveup (NonDataChunkIsTooLong tag)
          ("fmt ", Just body) ->
            liftGet (return $ readWaveFmt wave body) >>= go
          (tag, Just body) ->
            go wave { waveOtherChunks = (tag, body) : waveOtherChunks wave }
  go def { waveFileFormat = WaveVanilla }

-- | Parse WAVE format chunk from given 'ByteString'. Return error is 'Left'
-- in case of failure.

readWaveFmt :: Wave -> ByteString -> Either String Wave
readWaveFmt wave bytes = flip S.runGet bytes $ do
  format <- S.getWord16le
  unless ( format == waveFormatPcm       ||
           format == waveFormatIeeeFloat ||
           format == waveFormatExtensible ) $
    fail "Unsupported audio format specified in fmt chunk"
  let extensible = format == waveFormatExtensible
      ieeeFloat  = format == waveFormatIeeeFloat
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
  when (ieeeFloat && bitsPerSample /= 32) $
    fail "The sample format is IEEE Float, but bits per sample is not 32"
  channelMask <- if extensible
    then fromSpeakerMask <$> S.getWord32le
    else return (defaultSpeakerSet channels)
  return wave
    { waveSampleRate    = sampleRate
    , waveSampleFormat  =
      if ieeeFloat
        then SampleFormatIeeeFloat
        else if bitsPerSample <= 8
               then SampleFormatPcmUnsigned bitsPerSample
               else SampleFormatPcmSigned   bitsPerSample
    , waveChannelMask   = channelMask }

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

writeWaveFile :: MonadIO m
  => FilePath          -- ^ Where to save the file
  -> Wave              -- ^ Parameters of the WAVE file
  -> (Handle -> IO ()) -- ^ Callback that will be used to write WAVE data
  -> m ()
writeWaveFile path wave writeData = liftIO . withFile path WriteMode $ \h -> do
  let writeNoData = (Left . const . return) ()
      writeBsChunk chunkTag body =
        let chunkSize = fromIntegral (B.length body)
            chunkBody = Right body
        in writeChunk h Chunk {..}
  -- Write the outer RIFF chunk.
  beforeOuter <- hTell h
  writeChunk h (Chunk "RIFF" 0 writeNoData)
  -- Write the WAVE format tag.
  B.hPut h "WAVE"
  -- Write fmt chunk.
  writeBsChunk "fmt " (renderFmtChunk wave)
  -- Write any extra chunks if present.
  forM_ (waveOtherChunks wave) (uncurry writeBsChunk)
  -- Write data chunk.
  beforeData <- hTell h
  writeChunk h (Chunk "data" 0 (Left writeData))
  -- Take care of alignment.
  rightAfterData <- hTell h
  when (odd rightAfterData) $
    B.hPut h "\0"
  -- Go back and write sizes.
  afterData  <- hTell h
  hSeek h AbsoluteSeek beforeData
  writeChunk h (Chunk "data" (fromIntegral $ afterData - beforeData) writeNoData)
  hSeek h AbsoluteSeek beforeOuter
  writeChunk h (Chunk "RIFF" (fromIntegral $ afterData - beforeOuter) writeNoData)

-- | Render format chunk as a strict 'ByteString' from a given 'Wave'.

renderFmtChunk :: Wave -> ByteString
renderFmtChunk wave@Wave {..} = S.runPut $ do
  let extensible = isExtensibleFmt wave
      fmt = case waveSampleFormat of
        SampleFormatPcmUnsigned _ -> waveFormatPcm
        SampleFormatPcmSigned   _ -> waveFormatPcm
        SampleFormatIeeeFloat     -> waveFormatIeeeFloat
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
      SampleFormatPcmUnsigned _ -> ksdataformatSubtypePcm
      SampleFormatPcmSigned   _ -> ksdataformatSubtypePcm
      SampleFormatIeeeFloat     -> ksdataformatSubtypeIeeeFloat

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
ksdataformatSubtypePcm =
  "\x00\x00\x00\x01\x00\x00\x00\x10\x80\x00\x00\xaa\x00\x38\x9b\x71"

-- | GUID for extensible format chunk corresponding to IEEE float.

ksdataformatSubtypeIeeeFloat :: ByteString -- KSDATAFORMAT_SUBTYPE_IEEE_FLOAT
ksdataformatSubtypeIeeeFloat =
  "\x00\x00\x00\x03\x00\x00\x00\x10\x80\x00\x00\xaa\x00\x38\x9b\x71"

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
defaultSpeakerSet = E.fromList . f . fromIntegral
  where
    f n = case n of
      0 -> []
      1 -> [SpeakerFrontCenter]
      2 -> [SpeakerFrontLeft,SpeakerFrontRight]
      3 -> [SpeakerFrontLeft,SpeakerFrontCenter,SpeakerFrontRight]
      4 -> [SpeakerFrontLeft,SpeakerFrontRight,SpeakerBackLeft,SpeakerBackRight]
      x -> take x [minBound..maxBound]

-- | Does this 'Wave' record requires extensible format chunk to be used?

isExtensibleFmt :: Wave -> Bool
isExtensibleFmt wave@Wave {..} =
  waveChannels wave > 2 ||
  waveChannelMask /= defaultSpeakerSet (waveChannels wave) ||
  (waveBitsPerSample wave `rem` 8) /= 0

-- | Round bits per sample to next multiplier of 8, if necessary.

roundBitsPerSample :: Word16 -> Word16
roundBitsPerSample n = if r /= 0 then (x + 1) * 8 else n
  where
    (x,r) = n `quotRem` 8
