{-# LANGUAGE CPP, MagicHash #-}

-- |
-- Module      : Data.Text.Internal.Unsafe.Char
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
-- Fast character manipulation functions.
module Data.Text.Internal.Unsafe.Char
    (
      ord
    , unsafeChr16
    , unsafeChr8
    , unsafeChr32
    , unsafeWrite
    ) where

import Control.Monad.ST (ST)
import Data.Text.Internal.Encoding.Utf8
import GHC.Exts (Char(..), Int(..), chr#, ord#, word2Int#)
import GHC.Word (Word8(..), Word16(..), Word32(..))
import qualified Data.Text.Array as A
import Data.Text.Internal.PrimCompat ( word8ToWord#, word16ToWord#, word32ToWord# )
#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif

ord :: Char -> Int
ord (C# c#) = I# (ord# c#)
{-# INLINE ord #-}

unsafeChr16 :: Word16 -> Char
unsafeChr16 (W16# w#) = C# (chr# (word2Int# (word16ToWord# w#)))
{-# INLINE unsafeChr16 #-}

unsafeChr8 :: Word8 -> Char
unsafeChr8 (W8# w#) = C# (chr# (word2Int# (word8ToWord# w#)))
{-# INLINE unsafeChr8 #-}

unsafeChr32 :: Word32 -> Char
unsafeChr32 (W32# w#) = C# (chr# (word2Int# (word32ToWord# w#)))
{-# INLINE unsafeChr32 #-}

-- | Write a character into the array at the given offset.  Returns
-- the number of 'Word16's written.
unsafeWrite ::
#if defined(ASSERTS)
    HasCallStack =>
#endif
    A.MArray s -> Int -> Char -> ST s Int
unsafeWrite marr i c = do
    let l = utf8Length c
    case l of
        1 -> do
            let n0 = intToWord8 (ord c)
            A.unsafeWrite marr i n0
        2 -> do
            let n01 = ord2_16 c
            A.unsafeWrite16 marr i n01
        3 -> do
            let (n01, n2) = ord3_16_8 c
            A.unsafeWrite16 marr i n01
            A.unsafeWrite marr (i+2) n2
        _ -> do
            let n0123 = ord4_32 c
            A.unsafeWrite32 marr i n0123
    return l
{-# INLINE unsafeWrite #-}

intToWord8 :: Int -> Word8
intToWord8 = fromIntegral
