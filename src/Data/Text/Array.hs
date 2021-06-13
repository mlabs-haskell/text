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
    , shrinkM
    , copyM
    , copyI
    , empty
    , equal
    , run
    , run2
    , toList
    , unsafeFreeze
    , unsafeIndex
    , new
    , unsafeWrite
    ) where

#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif
#if !MIN_VERSION_base(4,11,0)
import Data.Text.Internal.Unsafe (inlinePerformIO)
#endif
import GHC.Exts hiding (toList)
import GHC.ST (ST(..), runST)
import GHC.Word (Word8(..))
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

shrinkM ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  MArray s -> Int -> ST s ()
shrinkM (MArray marr) i@(I# newSize) = do
#if defined(ASSERTS)
  oldSize <- getSizeofMArray (MArray marr)
  if I# newSize > oldSize
    then error $ "shrinkM: shrink cannot grow " ++ show oldSize ++ " to " ++ show (I# newSize)
    else return ()
#endif
  ST $ \s1# ->
    case shrinkMutableByteArray# marr newSize s1# of
      s2# -> (# s2#, () #)

-- | Copy some elements of a mutable array.
copyM :: MArray s               -- ^ Destination
      -> Int                    -- ^ Destination offset
      -> MArray s               -- ^ Source
      -> Int                    -- ^ Source offset
      -> Int                    -- ^ Count
      -> ST s ()
copyM dst@(MArray dst#) dstOff@(I# dstOff#) src@(MArray src#) srcOff@(I# srcOff#) count@(I# count#)
#if defined(ASSERTS)
  | count < 0 = error $
    "copyM: count must be >= 0, but got " ++ show count
#endif
    | otherwise = do
#if defined(ASSERTS)
    srcLen <- getSizeofMArray src
    dstLen <- getSizeofMArray dst
    if srcOff + count > srcLen
      then error "copyM: source is too short"
      else return ()
    if dstOff + count > dstLen
      then error "copyM: destination is too short"
      else return ()
#endif
    ST $ \s1# -> case copyMutableByteArray# src# srcOff# dst# dstOff# count# s1# of
      s2# -> (# s2#, () #)
{-# INLINE copyM #-}

-- | Copy some elements of an immutable array.
copyI :: MArray s               -- ^ Destination
      -> Int                    -- ^ Destination offset
      -> Array                  -- ^ Source
      -> Int                    -- ^ Source offset
      -> Int                    -- ^ Count
      -> ST s ()
copyI (MArray dst#) dstOff@(I# dstOff#) (Array src#) (I# srcOff#) count@(I# count#)
#if defined(ASSERTS)
  | count < 0 = error $
    "copyI: count must be >= 0, but got " ++ show count
#endif
  | otherwise = ST $ \s1# ->
    case copyByteArray# src# srcOff# dst# dstOff# count# s1# of
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
equal (Array src1#) (I# off1#) (Array src2#) (I# off2#) (I# count#) = i == 0
  where
#if MIN_VERSION_base(4,11,0)
    i = I# (compareByteArrays# src1# off1# src2# off2# count#)
#else
    i = inlinePerformIO (memcmp src1# off1# src2# off2# count#)

foreign import ccall unsafe "_hs_text_memcmp" memcmp
    :: ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> IO Int
#endif
{-# INLINE equal #-}
