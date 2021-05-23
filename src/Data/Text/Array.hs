{-# LANGUAGE BangPatterns, CPP, MagicHash, Rank2Types,
    RecordWildCards, UnboxedTuples, UnliftedFFITypes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
-- |
-- Module      : Data.Text.Array
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : portable
--
-- Packed, unboxed, heap-resident arrays.  Suitable for performance
-- critical use, both in terms of large data quantities and high
-- speed.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions, e.g.
--
-- > import qualified Data.Text.Array as A
--
-- The names in this module resemble those in the 'Data.Array' family
-- of modules, but are shorter due to the assumption of qualified
-- naming.
module Data.Text.Array
    (
    -- * Types
      Array(..)
    , MArray(..)
    -- * Functions
    , resizeM
    , copyM
    , copyI
    , empty
    , equal
    , run
    , run2
    , toList
    , unsafeFreeze
    , unsafeIndex
    , unsafeIndex16
    , unsafeIndex32
    , new
    , unsafeWrite
    , unsafeWrite16
    , unsafeWrite32
    ) where

#if defined(ASSERTS)
-- TODO employ resizeMutableByteArray# instead of cropping Text
import GHC.Stack (HasCallStack)
#endif
import Data.Text.Internal.Unsafe (inlinePerformIO)
import Foreign.C.Types (CInt(CInt), CSize(CSize))
import GHC.Exts hiding (toList)
import GHC.ST (ST(..), runST)
import GHC.Word (Word8(..), Word16(..), Word32(..))
import Prelude hiding (length, read)

-- | Immutable array type.
--
-- The 'Array' constructor is exposed since @text-1.1.1.3@
data Array = Array { aBA :: ByteArray# }

instance Show Array where
  show a@(Array a#) = show $ toList a 0 (I# (sizeofByteArray# a#))

-- | Mutable array type, for use in the ST monad.
--
-- The 'MArray' constructor is exposed since @text-1.1.1.3@
data MArray s = MArray { maBA :: MutableByteArray# s }

-- | Create an uninitialized mutable array.
new :: forall s. Int -> ST s (MArray s)
new (I# len#)
#if defined(ASSERTS)
  | I# len# < 0 = error "Data.Text.Array.new: size overflow"
#endif
  | otherwise = ST $ \s1# ->
    case newByteArray# len# s1# of
      (# s2#, marr# #) -> (# s2#, MArray marr# #)
{-# INLINE new #-}

-- | Freeze a mutable array. Do not mutate the 'MArray' afterwards!
unsafeFreeze :: MArray s -> ST s Array
unsafeFreeze MArray{..} = ST $ \s1# ->
    case unsafeFreezeByteArray# maBA s1# of
        (# s2#, ba# #) -> (# s2#, Array ba# #)
{-# INLINE unsafeFreeze #-}

-- | Unchecked read of an immutable array.  May return garbage or
-- crash on an out-of-bounds access.
unsafeIndex ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Array -> Int -> Word8
unsafeIndex a@Array{..} i@(I# i#) =
#if defined(ASSERTS)
  let word8len = I# (sizeofByteArray# aBA) in
  if i < 0 || i >= word8len
  then error ("Data.Text.Array.unsafeIndex: bounds error, offset " ++ show i ++ ", length " ++ show word8len)
  else
#endif
  case indexWord8Array# aBA i# of r# -> (W8# r#)
{-# INLINE unsafeIndex #-}

unsafeIndex16 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Array -> Int -> Word16
unsafeIndex16 a@Array{..} i@(I# i#) =
#if defined(ASSERTS)
  let word8len = I# (sizeofByteArray# aBA) in
  if i < 0 || i >= word8len - 1
  then error ("Data.Text.Array.unsafeIndex16: bounds error, offset " ++ show i ++ ", length " ++ show word8len)
  else
#endif
  case indexWord8ArrayAsWord16# aBA i# of r# -> (W16# r#)
{-# INLINE unsafeIndex16 #-}

unsafeIndex32 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Array -> Int -> Word32
unsafeIndex32 a@Array{..} i@(I# i#) =
#if defined(ASSERTS)
  let word8len = I# (sizeofByteArray# aBA) in
  if i < 0 || i >= word8len - 3
  then error ("Data.Text.Array.unsafeIndex32: bounds error, offset " ++ show i ++ ", length " ++ show word8len)
  else
#endif
  case indexWord8ArrayAsWord32# aBA i# of r# -> (W32# r#)
{-# INLINE unsafeIndex32 #-}

#if defined(ASSERTS)
-- sizeofMutableByteArray# is deprecated, because it is unsafe in the presence of
-- shrinkMutableByteArray# and resizeMutableByteArray#.
getSizeofMArray :: MArray s -> ST s Int
getSizeofMArray ma@MArray{..} = ST $ \s0# ->
  case getSizeofMutableByteArray# maBA s0# of
    (# s1#, word8len# #) -> (# s1#, I# word8len# #)

checkBoundsM :: HasCallStack => MArray s -> Int -> Int -> ST s ()
checkBoundsM ma i elSize = do
  len <- getSizeofMArray ma
  if i < 0 || i + elSize > len
    then error ("bounds error, offset " ++ show i ++ ", length " ++ show len)
    else return ()
#endif

-- | Unchecked write of a mutable array.  May return garbage or crash
-- on an out-of-bounds access.
unsafeWrite ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  MArray s -> Int -> Word8 -> ST s ()
unsafeWrite ma@MArray{..} i@(I# i#) (W8# e#) =
#if defined(ASSERTS)
  checkBoundsM ma i 1 >>
#endif
  (ST $ \s1# -> case writeWord8Array# maBA i# e# s1# of
    s2# -> (# s2#, () #))
{-# INLINE unsafeWrite #-}

unsafeWrite16 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  MArray s -> {- offset in bytes! -} Int -> Word16 -> ST s ()
unsafeWrite16 ma@MArray{..} i@(I# i#) (W16# e#) =
#if defined(ASSERTS)
  checkBoundsM ma i 2 >>
#endif
  (ST $ \s1# -> case writeWord8ArrayAsWord16# maBA i# e# s1# of
    s2# -> (# s2#, () #))
{-# INLINE unsafeWrite16 #-}

unsafeWrite32 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  MArray s -> {- offset in bytes! -} Int -> Word32 -> ST s ()
unsafeWrite32 ma@MArray{..} i@(I# i#) (W32# e#) =
#if defined(ASSERTS)
  checkBoundsM ma i 4 >>
#endif
  (ST $ \s1# -> case writeWord8ArrayAsWord32# maBA i# e# s1# of
    s2# -> (# s2#, () #))
{-# INLINE unsafeWrite32 #-}

-- | Convert an immutable array to a list.
toList :: Array -> Int -> Int -> [Word8]
toList ary off len = loop 0
    where loop i | i < len   = unsafeIndex ary (off+i) : loop (i+1)
                 | otherwise = []

-- | An empty immutable array.
empty :: Array
empty = runST (new 0 >>= unsafeFreeze)

-- | Run an action in the ST monad and return an immutable array of
-- its result.
run :: (forall s. ST s (MArray s)) -> Array
run k = runST (k >>= unsafeFreeze)

-- | Run an action in the ST monad and return an immutable array of
-- its result paired with whatever else the action returns.
run2 :: (forall s. ST s (MArray s, a)) -> (Array, a)
run2 k = runST (do
                 (marr,b) <- k
                 arr <- unsafeFreeze marr
                 return (arr,b))
{-# INLINE run2 #-}

resizeM :: MArray s -> Int -> ST s (MArray s)
resizeM ma@MArray{..} i@(I# i#) = ST $ \s1# ->
  case resizeMutableByteArray# maBA i# s1# of
    (# s2#, newArr #) -> (# s2#, MArray newArr #)

-- | Copy some elements of a mutable array.
copyM :: MArray s               -- ^ Destination
      -> Int                    -- ^ Destination offset
      -> MArray s               -- ^ Source
      -> Int                    -- ^ Source offset
      -> Int                    -- ^ Count
      -> ST s ()
copyM (MArray dst) (I# dstOff) (MArray src) (I# srcOff) (I# count)
#if defined(ASSERTS)
  | I# count < 0 = error $
    "copyM: count must be >= 0, but got " ++ show (I# count)
#endif
  | otherwise = do
#if defined(ASSERTS)
    srcLen <- getSizeofMArray (MArray src)
    dstLen <- getSizeofMArray (MArray dst)
    if I# srcOff + I# count > srcLen
      then error "copyM: source is too short"
      else return ()
    if I# dstOff + I# count > dstLen
      then error "copyM: destination is too short"
      else return ()
#endif
    ST $ \s1# -> case copyMutableByteArray# src srcOff dst dstOff count s1# of
      s2# -> (# s2#, () #)
{-# INLINE copyM #-}

-- | Copy some elements of an immutable array.
copyI :: MArray s               -- ^ Destination
      -> Int                    -- ^ Destination offset
      -> Array                  -- ^ Source
      -> Int                    -- ^ Source offset
      -> Int                    -- ^ Count
      -> ST s ()
copyI (MArray dst) (I# dstOff) (Array src) (I# srcOff) (I# count)
#if defined(ASSERTS)
  | I# count < 0 = error $
    "copyI: count must be >= 0, but got " ++ show (I# count)
#endif
  | otherwise = ST $ \s1# ->
    case copyByteArray# src srcOff dst dstOff count s1# of
      s2# -> (# s2#, () #)
{-# INLINE copyI #-}

-- | Compare portions of two arrays for equality.  No bounds checking
-- is performed.
equal :: Array                  -- ^ First
      -> Int                    -- ^ Offset into first
      -> Array                  -- ^ Second
      -> Int                    -- ^ Offset into second
      -> Int                    -- ^ Count
      -> Bool
equal arrA offA arrB offB count = inlinePerformIO $ do
  i <- memcmp (aBA arrA) (intToCSize offA)
                     (aBA arrB) (intToCSize offB) (intToCSize count)
  return $! i == 0
{-# INLINE equal #-}

intToCSize :: Int -> CSize
intToCSize = fromIntegral

foreign import ccall unsafe "_hs_text_memcmp" memcmp
    :: ByteArray# -> CSize -> ByteArray# -> CSize -> CSize -> IO CInt
