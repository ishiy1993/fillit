{-# LANGUAGE OverloadedStrings #-}
module FillitInternalSpec where

import Test.Hspec

import Data.Text.Fillit.Internal

spec :: Spec
spec = describe "Internal functions" $ do
    it "checks between'" $
        parseOnly (between' "$" "$") "$name$"
            `shouldBe` Right "name"
    it "checks parseTemplate in a simple case" $
        parseTemplate def "$name$さん"
            `shouldBe` Right [Req "name", Raw "さん"]
    it "checks parseTemplate in another simple case" $
        parseTemplate def "%school%$name$さん"
            `shouldBe` Right [Opt "school", Req "name", Raw "さん"]
    it "checks parseTemplate in yet another simple case" $
        parseTemplate def "訪問: $date$"
            `shouldBe` Right [Raw "訪問: ", Req "date"]
    it "checks parseTemplate in a multi-line case" $
        parseTemplate def "%school%$name$さん\n\n## 背景\n%note%"
            `shouldBe` Right [ Opt "school"
                             , Req "name"
                             , Raw "さん\n\n## 背景\n"
                             , Opt "note"]
    it "checks parseTemplate in a invalid case" $
        parseTemplate def "$name さん"
            `shouldNotBe` Right []
    it "checks parseTemplate in another invalid case" $
        parseTemplate def "100%"
            `shouldNotBe` Right []
