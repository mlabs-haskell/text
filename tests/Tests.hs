{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Text as S
import Language.Haskell.TH.Syntax (lift)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)

main :: IO ()
main = defaultMain $ testGroup "TH lifting Text"
  [ testCase "strict" $ assertEqual "strict"
      $(lift (S.Text "foo" :: S.Text))
      (S.Text "foo" :: S.Text)
  ]
