{-# LANGUAGE CPP #-}

module Main
    ( main
    ) where

import Test.Tasty (defaultMain, testGroup)

import qualified Tests.Properties as Properties
import qualified Tests.Regressions as Regressions

#ifndef mingw32_HOST_OS
import qualified Tests.Lift as Lift
#endif

#ifndef ASSERTS
import qualified Tests.Inspection.Strict as InspectionStrict
import qualified Tests.Inspection.Lazy   as InspectionLazy
#endif

main :: IO ()
main = defaultMain $ testGroup "All"
  [ Properties.tests
  , Regressions.tests
#ifndef mingw32_HOST_OS
  , Lift.tests
#endif
#if !defined(ASSERTS)
  , InspectionStrict.tests
  , InspectionLazy.tests
#endif
  ]
