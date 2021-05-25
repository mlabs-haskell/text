{-# LANGUAGE BangPatterns, CPP, MagicHash, Rank2Types, UnboxedTuples, TypeFamilies #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Data.Text
    ( Text(..)
    ) where

import Language.Haskell.TH.Syntax
import GHC.Exts
import Foreign.C

newtype Text = Text String
    deriving (Eq, Show, Lift)

foreign import ccall unsafe "_hs_text_iterN" c_iterN
    :: ByteArray# -> CSize -> CSize -> CSize -> IO CSize
