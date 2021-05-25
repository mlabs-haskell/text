{-# LANGUAGE TemplateHaskell #-}

module TextLift (foo) where

import Text
import Language.Haskell.TH.Syntax (lift)

foo :: Text
foo = $(lift (Text "foo"))
