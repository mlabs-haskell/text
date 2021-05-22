{-# LANGUAGE CPP, MagicHash, BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}

-- |
-- Module      : Data.Text.Internal.Encoding.Utf8
-- Copyright   : (c) 2008, 2009 Tom Harper,
--               (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Basic UTF-8 validation and character manipulation.
module Data.Text.Internal.Encoding.Utf8
    ( utf8Length
    , utf8LengthByLeader
    -- Decomposition
    , ord2
    , ord2_16
    , ord3
    , ord3_16_8
    , ord4
    , ord4_32
    -- Construction
    , chr2
    , chr2_16
    , chr3
    , chr3_8_16
    , chr4
    , chr4_32
    -- * Validation
    , validate1
    , validate2
    , validate3
    , validate4
    ) where

import Data.Bits (Bits(..), FiniteBits(..))
import Data.Char (ord)
import GHC.ByteOrder
import GHC.Exts
import GHC.Word (Word8(..), Word16(..), Word32(..), Word64)

#if !MIN_VERSION_base(4,16,0)
-- harmless to import, except for warnings that it is unused.
import Data.Text.Internal.PrimCompat (word8ToWord#, word16ToWord#, word32ToWord#)
#endif

default(Int)

between :: Word8                -- ^ byte to check
        -> Word8                -- ^ lower bound
        -> Word8                -- ^ upper bound
        -> Bool
between x y z = x >= y && x <= z
{-# INLINE between #-}

-- This is a branchless version of
-- utf8Length c
--   | ord c < 0x80    = 1
--   | ord c < 0x800   = 2
--   | ord c < 0x10000 = 3
--   | otherwise       = 4
utf8Length :: Char -> Int
utf8Length c = word64ToInt $ 4 - (magic `unsafeShiftR` wordToInt (bitLength `shiftL` 1) .&. 3)
  where
    bitLength :: Word
    bitLength = intToWord (finiteBitSize (0 :: Int)) - intToWord (countLeadingZeros (ord c))
    magic :: Word64
    magic = 0b0101010101101010101111111111111111

--- This is a version of
--- utf8LengthByLeader w
---   | w < 0x80  = 1
---   | w < 0xE0  = 2
---   | w < 0xF0  = 3
---   | otherwise = 4
utf8LengthByLeader :: Word8 -> Int
utf8LengthByLeader w = max 1 (countLeadingZeros (maxBound - w))

ord2' :: Char -> (Int, Int)
ord2' c = (x1, x2)
  where
    n  = ord c
    x1 = (n `shiftR` 6) + 0xC0
    x2 = (n .&. 0x3F)   + 0x80

ord2 :: Char -> (Word8, Word8)
ord2 c = (intToWord8 x1, intToWord8 x2)
  where
    (x1, x2) = ord2' c

ord2_16 :: Char -> Word16
ord2_16 c = intToWord16 $ case targetByteOrder of
  BigEndian    -> (x1 `shiftL` 8) + x2
  LittleEndian -> x1 + (x2 `shiftL` 8)
  where
    (x1, x2) = ord2' c

ord3' :: Char -> (Int, Int, Int)
ord3' c = (x1, x2, x3)
  where
    n  = ord c
    x1 = (n `shiftR` 12) + 0xE0
    x2 = ((n `shiftR` 6) .&. 0x3F) + 0x80
    x3 = (n .&. 0x3F) + 0x80

ord3 :: Char -> (Word8, Word8, Word8)
ord3 c = (intToWord8 x1, intToWord8 x2, intToWord8 x3)
  where
    (x1, x2, x3) = ord3' c

ord3_16_8 :: Char -> (Word16, Word8)
ord3_16_8 c = (intToWord16 $ case targetByteOrder of
  BigEndian    -> (x1 `shiftL` 8) + x2
  LittleEndian -> x1 + (x2 `shiftL` 8), intToWord8 x3)
  where
    (x1, x2, x3) = ord3' c

ord4' :: Char -> (Int, Int, Int, Int)
ord4' c = (x1, x2, x3, x4)
  where
    n  = ord c
    x1 = (n `shiftR` 18) + 0xF0
    x2 = ((n `shiftR` 12) .&. 0x3F) + 0x80
    x3 = ((n `shiftR` 6) .&. 0x3F) + 0x80
    x4 = (n .&. 0x3F) + 0x80

ord4 :: Char -> (Word8, Word8, Word8, Word8)
ord4 c = (intToWord8 x1, intToWord8 x2, intToWord8 x3, intToWord8 x4)
  where
    (x1, x2, x3, x4) = ord4' c

ord4_32 :: Char -> Word32
ord4_32 c = intToWord32 $ case targetByteOrder of
  BigEndian    -> (x1 `shiftL` 24) + (x2 `shiftL` 16) + (x3 `shiftL` 8) + x4
  LittleEndian -> x1 + (x2 `shiftL` 8) + (x3 `shiftL` 16) + (x4 `shiftL` 24)
  where
    (x1, x2, x3, x4) = ord4' c

chr2' :: Int -> Int -> Char
chr2' (I# y1#) (I# y2#) = C# (chr# (z1# +# z2#))
    where
      !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
      !z2# = y2# -# 0x80#
{-# INLINE chr2' #-}

chr2 :: Word8 -> Word8 -> Char
chr2 (W8# x1#) (W8# x2#) = chr2' (I# y1#) (I# y2#)
    where
      !y1# = word2Int# (word8ToWord# x1#)
      !y2# = word2Int# (word8ToWord# x2#)
{-# INLINE chr2 #-}

chr2_16 :: Word16 -> Char
chr2_16 (W16# x#) = case targetByteOrder of
  BigEndian    -> chr2' (I# y# `shiftR` 8) (I# y# .&. 0xFF)
  LittleEndian -> chr2' (I# y# .&. 0xFF) (I# y# `shiftR` 8)
  where
    !y# = word2Int# (word16ToWord# x#)
{-# INLINE chr2_16 #-}

chr3' :: Int -> Int -> Int -> Char
chr3' (I# y1#) (I# y2#) (I# y3#) = C# (chr# (z1# +# z2# +# z3#))
    where
      !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
      !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
      !z3# = y3# -# 0x80#
{-# INLINE chr3' #-}

chr3 :: Word8 -> Word8 -> Word8 -> Char
chr3 (W8# x1#) (W8# x2#) (W8# x3#) = chr3' (I# y1#) (I# y2#) (I# y3#)
    where
      !y1# = word2Int# (word8ToWord# x1#)
      !y2# = word2Int# (word8ToWord# x2#)
      !y3# = word2Int# (word8ToWord# x3#)
{-# INLINE chr3 #-}

chr3_8_16 :: Word8 -> Word16 -> Char
chr3_8_16 (W8# x0#) (W16# x1#) = case targetByteOrder of
  BigEndian    -> chr3' (I# y0#) (I# y1# `shiftR` 8) (I# y1# .&. 0xFF)
  LittleEndian -> chr3' (I# y0#) (I# y1# .&. 0xFF) (I# y1# `shiftR` 8)
  where
    !y0# = word2Int# (word8ToWord# x0#)
    !y1# = word2Int# (word16ToWord# x1#)
{-# INLINE chr3_8_16 #-}

chr4' :: Int -> Int -> Int -> Int -> Char
chr4' (I# y1#) (I# y2#) (I# y3#) (I# y4#) =
    C# (chr# (z1# +# z2# +# z3# +# z4#))
    where
      !z1# = uncheckedIShiftL# (y1# -# 0xF0#) 18#
      !z2# = uncheckedIShiftL# (y2# -# 0x80#) 12#
      !z3# = uncheckedIShiftL# (y3# -# 0x80#) 6#
      !z4# = y4# -# 0x80#
{-# INLINE chr4' #-}

chr4 :: Word8 -> Word8 -> Word8 -> Word8 -> Char
chr4 (W8# x1#) (W8# x2#) (W8# x3#) (W8# x4#) =
  chr4' (I# y1#) (I# y2#) (I# y3#) (I# y4#)
    where
      !y1# = word2Int# (word8ToWord# x1#)
      !y2# = word2Int# (word8ToWord# x2#)
      !y3# = word2Int# (word8ToWord# x3#)
      !y4# = word2Int# (word8ToWord# x4#)
{-# INLINE chr4 #-}

chr4_32 :: Word32 -> Char
chr4_32 (W32# x#) = case targetByteOrder of
  BigEndian    -> chr4' (I# y# `shiftR` 24) ((I# y# `shiftR` 16) .&. 0xFF) ((I# y# `shiftR` 8) .&. 0xFF) (I# y# .&. 0xFF)
  LittleEndian -> chr4' (I# y# .&. 0xFF) ((I# y# `shiftR` 8) .&. 0xFF) ((I# y# `shiftR` 16) .&. 0xFF) (I# y# `shiftR` 24)
  where
    !y# = word2Int# (word32ToWord# x#)
{-# INLINE chr4_32 #-}

validate1 :: Word8 -> Bool
validate1 x1 = x1 <= 0x7F
{-# INLINE validate1 #-}

validate2 :: Word8 -> Word8 -> Bool
validate2 x1 x2 = between x1 0xC2 0xDF && between x2 0x80 0xBF
{-# INLINE validate2 #-}

validate3 :: Word8 -> Word8 -> Word8 -> Bool
{-# INLINE validate3 #-}
validate3 x1 x2 x3 = validate3_1 || validate3_2 || validate3_3 || validate3_4
  where
    validate3_1 = (x1 == 0xE0) &&
                  between x2 0xA0 0xBF &&
                  between x3 0x80 0xBF
    validate3_2 = between x1 0xE1 0xEC &&
                  between x2 0x80 0xBF &&
                  between x3 0x80 0xBF
    validate3_3 = x1 == 0xED &&
                  between x2 0x80 0x9F &&
                  between x3 0x80 0xBF
    validate3_4 = between x1 0xEE 0xEF &&
                  between x2 0x80 0xBF &&
                  between x3 0x80 0xBF

validate4 :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
{-# INLINE validate4 #-}
validate4 x1 x2 x3 x4 = validate4_1 || validate4_2 || validate4_3
  where
    validate4_1 = x1 == 0xF0 &&
                  between x2 0x90 0xBF &&
                  between x3 0x80 0xBF &&
                  between x4 0x80 0xBF
    validate4_2 = between x1 0xF1 0xF3 &&
                  between x2 0x80 0xBF &&
                  between x3 0x80 0xBF &&
                  between x4 0x80 0xBF
    validate4_3 = x1 == 0xF4 &&
                  between x2 0x80 0x8F &&
                  between x3 0x80 0xBF &&
                  between x4 0x80 0xBF

intToWord8 :: Int -> Word8
intToWord8 = fromIntegral

intToWord16 :: Int -> Word16
intToWord16 = fromIntegral

intToWord32 :: Int -> Word32
intToWord32 = fromIntegral

intToWord :: Int -> Word
intToWord = fromIntegral

word64ToInt :: Word64 -> Int
word64ToInt = fromIntegral

wordToInt :: Word -> Int
wordToInt = fromIntegral
