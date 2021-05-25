{-# LANGUAGE DeriveLift #-}

module Text
    ( Text(..)
    , c_bar
    ) where

import Data.Word
import GHC.Exts
import Language.Haskell.TH.Syntax

newtype Text = Text String
    deriving (Eq, Show, Lift)

foreign import ccall unsafe "_hs_bar" c_bar
    :: Word64 -> Word64
