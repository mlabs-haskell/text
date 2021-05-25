{-# LANGUAGE TemplateHaskell #-}

module Data.TextLift (foo) where

import Data.Text
import Language.Haskell.TH.Syntax (lift)

foo :: Text
foo = $(lift (Text "foo" :: Text))
