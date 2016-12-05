-- |
-- Module      :  Codec.Audio.Wave
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- TODO

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Codec.Audio.Wave
  ( -- * Types
    Wave (..)
  , WaveFormat (..)
  , WaveException (..)
    -- * Reading
  , readWaveFile
    -- * Writing
  , writeWaveFile
    -- * Extra functionality
  , waveByteRate
  , waveBitRate
  , waveBlockAlign )
where

import Codec.Audio.Wave.Riff
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Data.ByteString (ByteString)
import Data.Default.Class
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Typeable
import Data.Word
import System.IO
import qualified Data.ByteString as B
import qualified Data.Serialize  as S
import qualified Data.Set        as E

-- TODO Full support of vanilla WAVE files
-- TODO Document stuff
-- TODO Test support for vanilla WAVE files, use samples from https://en.wikipedia.org/wiki/WAV
-- TODO Add support for RF64 https://tech.ebu.ch/docs/tech/tech3306-2009.pdf
-- TODO Add tests for RF64
-- TODO Add support for WAVE64 http://www.ambisonia.com/Members/mleese/sony_wave64.pdf/sony_wave64.pdf
-- TODO Add tests for WAVE64

----------------------------------------------------------------------------
-- Types

data Wave = Wave
  { waveFileFormat   :: !WaveFormat
  , waveSampleRate   :: !Word32
  , waveSampleFormat :: !SampleFormat
  , waveChannelMask  :: !(Set SpeakerPosition)
  , waveDataSize     :: !Word64 -- to allow RF64 and WAVE64
  , waveDataOffset   :: !Word64 -- to allow RF64 and WAVE64
  , waveOtherChunks  :: [(ByteString, ByteString)]
  }

instance Default Wave where
  def = Wave
    { waveFileFormat   = WaveVanilla
    , waveSampleRate   = 44100
    , waveSampleFormat = SampleFormatPcmSigned 16
    , waveChannelMask  = defaultSpeakerSet 2
    , waveDataSize     = 0
    , waveDataOffset   = 0
    , waveOtherChunks  = [] }

data WaveFormat
  = WaveVanilla
  | WaveRF64
  | Wave64
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

data SampleFormat
  = SampleFormatPcmUnsigned Word16
  | SampleFormatPcmSigned   Word16
  | SampleFormatIeeeFloat
  deriving (Show, Read, Eq, Ord)

data SpeakerPosition
  = SpeakerFrontLeft
  | SpeakerFrontRight
  | SpeakerFrontCenter
  | SpeakerLowFrequency
  | SpeakerBackLeft
  | SpeakerBackRight
  | SpeakerFrontLeftOfCenter
  | SpeakerFrontRightOfCenter
  | SpeakerBackCenter
  | SpeakerSideLeft
  | SpeakerSideRight
  | SpeakerTopCenter
  | SpeakerTopFrontLeft
  | SpeakerTopFrontCenter
  | SpeakerTopFrontRight
  | SpeakerTopBackLeft
  | SpeakerTopBackCenter
  | SpeakerTopBackRight
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

data WaveException
  = UnknownFileFormat     String     FilePath
  | NonDataChunkIsTooLong ByteString FilePath
  deriving (Show, Read, Eq, Typeable) -- more?

instance Exception WaveException

----------------------------------------------------------------------------
-- Reading

readWaveFile :: MonadIO m => FilePath -> m Wave
readWaveFile path = liftIO . withFile path ReadMode $ \h -> do
  let giveup f = throwIO (f path)
  outerChunk <- liftGet path (readChunk h 0)
  unless (chunkTag outerChunk == "RIFF") $
    giveup (UnknownFileFormat "Can't locate the RIFF tag")
  let go wave = do
        offset <- hTell h
        Chunk {..} <- liftGet path (readChunk h 0xffff)
        case (chunkTag, chunkBody) of
          ("data", _) ->
            return wave { waveDataOffset = fromIntegral offset + 8 }
          (tag, Nothing) ->
            giveup (NonDataChunkIsTooLong tag)
          ("fmt ", Just body) ->
            liftGet path (return $ readWaveFmt wave body) >>= go
          (tag, Just body) ->
            go wave { waveOtherChunks = (tag, body) : waveOtherChunks wave }
  go def { waveFileFormat = WaveVanilla }

readWaveFmt :: Wave -> ByteString -> Either String Wave
readWaveFmt wave bytes = flip S.runGet bytes $ do
  format <- S.getWord16le
  unless ( format == wave_format_pcm        ||
           format == wave_format_ieee_float ||
           format == wave_format_extensible ) $
    fail "Unsupported audio format specified in fmt chunk"
  let extensible = format == wave_format_extensible
      ieeeFloat  = format == wave_format_ieee_float
  channels   <- S.getWord16le
  sampleRate <- S.getWord32le
  S.skip 4 -- byte rate (useless)
  S.skip 2 -- block align (useless)
  bps        <- S.getWord16le
  hasExtSize <- not <$> S.isEmpty
  extSize    <- if hasExtSize
    then S.getWord16le
    else return 0
  when (extSize < 22 && extensible) $
    fail "The format is specified as extensible, but it's shorter than 22 bytes"
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

----------------------------------------------------------------------------
-- Writing

writeWaveFile :: MonadIO m => FilePath -> Wave -> (Handle -> IO ()) -> m ()
writeWaveFile path wave writeData = liftIO . withFile path WriteMode $ \h -> do
  let writeNoData = (Left . const . return) ()
      writeBsChunk chunkTag body =
        let chunkSize = fromIntegral (B.length body)
            chunkBody = Right body
        in writeChunk h Chunk {..}
  -- Write the outer RIFF chunk.
  beforeOuter <- hTell h
  writeChunk h (Chunk "RIFF" 0 writeNoData)
  -- Write any extra chunks if present.
  forM_ (waveOtherChunks wave) (uncurry writeBsChunk)
  -- Write fmt chunk.
  writeBsChunk "fmt " (renderFmtChunk wave)
  -- Write data chunk.
  -- TODO apply WAVE alignment rules to waveDataSize here
  -- TODO another idea is not to believe waveDataSize, but check after
  -- writing and return/overwrite the size also add a pad byte for alignment
  -- if necessary
  beforeData <- hTell h
  writeChunk h (Chunk "data" 0 (Left writeData))
  -- FIXME here take care of alignment
  afterData  <- hTell h
  hSeek h AbsoluteSeek beforeData
  writeChunk h (Chunk "data" (fromIntegral $ afterData - beforeData) writeNoData)
  hSeek h AbsoluteSeek beforeOuter
  writeChunk h (Chunk "RIFF" (fromIntegral $ afterData - beforeOuter) writeNoData)

renderFmtChunk :: Wave -> ByteString
renderFmtChunk wave@Wave {..} = S.runPut $ do
  let extensible = isExtensibleFmt wave
      fmt = case waveSampleFormat of
        SampleFormatPcmUnsigned _ -> wave_format_pcm
        SampleFormatPcmSigned   _ -> wave_format_pcm
        SampleFormatIeeeFloat     -> wave_format_ieee_float
      bps = waveBitsPerSample wave
  S.putWord16le (if extensible then wave_format_extensible else fmt)
  S.putWord16le (waveChannels wave)
  S.putWord32le waveSampleRate
  S.putWord32le (waveByteRate wave)
  S.putWord16le (waveBlockAlign wave)
  S.putWord16le (normalizeBitsPerSample bps)
  when extensible $ do
    S.putWord16le 22
    S.putWord16le bps
    S.putWord32le (toSpeakerMask waveChannelMask)
    S.putByteString $ case waveSampleFormat of
      SampleFormatPcmUnsigned _ -> ksdataformat_subtype_pcm
      SampleFormatPcmSigned   _ -> ksdataformat_subtype_pcm
      SampleFormatIeeeFloat     -> ksdataformat_subtype_ieee_float

----------------------------------------------------------------------------
-- Extra functionality

waveByteRate :: Wave -> Word32
waveByteRate = undefined -- == SampleRate * NumChannels * BitsPerSample/8

waveBitRate :: Wave -> Word32
waveBitRate = undefined

waveBitsPerSample :: Wave -> Word16
waveBitsPerSample Wave {..} =
  case waveSampleFormat of
    SampleFormatPcmUnsigned bps -> bps
    SampleFormatPcmSigned   bps -> bps
    SampleFormatIeeeFloat       -> 32

waveBlockAlign :: Wave -> Word16
waveBlockAlign = undefined -- == NumChannels * BitsPerSample/8
                               -- The number of bytes for one sample including
                               -- all channels. I wonder what happens when
                               -- this number isn't an integer?

waveChannels :: Wave -> Word16
waveChannels Wave {..} =
  fromIntegral (E.size waveChannelMask)

----------------------------------------------------------------------------
-- Helpers

-- | Pulse-code modulation, vanilla WAVE.

wave_format_pcm :: Word16 -- WAVE_FORMAT_PCM
wave_format_pcm = 0x0001

wave_format_ieee_float :: Word16 -- WAVE_FORMAT_IEEE_FLOAT
wave_format_ieee_float = 0x0003

wave_format_extensible :: Word16
wave_format_extensible = 0xfffe -- WAVE_FORMAT_EXTENSIBLE

ksdataformat_subtype_pcm :: ByteString -- KSDATAFORMAT_SUBTYPE_PCM
ksdataformat_subtype_pcm =
  "\x00\x00\x00\x01\x00\x00\x00\x10\x80\x00\x00\xaa\x00\x38\x9b\x71"

ksdataformat_subtype_ieee_float :: ByteString -- KSDATAFORMAT_SUBTYPE_IEEE_FLOAT
ksdataformat_subtype_ieee_float =
  "\x00\x00\x00\x03\x00\x00\x00\x10\x80\x00\x00\xaa\x00\x38\x9b\x71"

liftGet :: FilePath -> IO (Either String a) -> IO a
liftGet path m = do
  r <- m
  case r of
    Left msg -> throwIO (UnknownFileFormat msg path)
    Right x -> return x

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
toSpeakerMask = E.foldl' (.&.) 0 . E.map speakerToFlag

-- | Transform a 4-byte mask into a set of 'SpeakerPosition's.

fromSpeakerMask :: Word32 -> Set SpeakerPosition
fromSpeakerMask channelMask = E.fromList $ mapMaybe f [minBound..maxBound]
  where
    f sp = if speakerToFlag sp .&. channelMask > 0
             then Just sp
             else Nothing

-- | The default speaker set with only left and right speakers.

defaultSpeakerSet :: Word16 -> Set SpeakerPosition
defaultSpeakerSet = E.fromList . f . fromIntegral
  where
    f n = case n of
      0 -> []
      1 -> [SpeakerFrontCenter]
      2 -> [SpeakerFrontLeft,SpeakerFrontRight]
      x -> take x [minBound..maxBound]

isExtensibleFmt :: Wave -> Bool
isExtensibleFmt wave@Wave {..} =
  waveChannels wave > 2 ||
  waveChannelMask /= defaultSpeakerSet (waveChannels wave) ||
  (waveBitsPerSample wave `rem` 8) /= 0

normalizeBitsPerSample :: Word16 -> Word16
normalizeBitsPerSample = undefined -- to near greater or equal multiply of 8
