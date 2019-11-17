{-# LANGUAGE NoImplicitPrelude #-}

module UtilSpec
  ( spec
  ) where

import Brainfxck.Tokenize
import Import
import qualified RIO.Vector.Boxed as VB
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "tokenize" $ do
    it "basic check" $
      tokenize "><+-.,[]" `shouldBe`
      VB.fromList
        [ GraterThan
        , LessThan
        , Plus
        , Minus
        , Dot
        , Comma
        , BracketLeft
        , BracketRight
        ]
    it "elem Comment" $
      tokenize "+a<bc->" `shouldBe`
      VB.fromList [Plus, LessThan, Minus, GraterThan]
    it "empty" $ tokenize "" `shouldBe` VB.fromList []
