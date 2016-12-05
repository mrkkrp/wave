-- |
-- Module      :  Codec.Audio.Wave.Riff
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This is an internal module providing support for reading and writing RIFF
-- chunks.
--
-- See also:
-- <https://en.wikipedia.org/wiki/Resource_Interchange_File_Format>.

{-# LANGUAGE RecordWildCards #-}

module Codec.Audio.Wave.Riff
  ( Chunk (..)
  , readChunk
  , writeChunk )
where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Word
import System.IO
import qualified Data.ByteString as B
import qualified Data.Serialize  as S

-- | A RIFF chunk allowing for different representations of its body.

data Chunk m = Chunk
  { chunkTag  :: ByteString
  , chunkSize :: Word32
  , chunkBody :: m ByteString
  }

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

