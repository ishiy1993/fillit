{-# LANGUAGE OverloadedStrings #-}
module FillitSpec where

import qualified Data.HashMap.Lazy as HM
import Test.Hspec

import Data.Text.Fillit

spec :: Spec
spec = do
    let dic = HM.fromList [("name", "太郎"), ("age", "22")] 
    describe "fill function" $ do
        it "checks fill in a simple case" $
            fill "$name$さん" dic
                `shouldBe` Right "太郎さん"
        it "checks fill in another simple case" $
            fill "%school% $name$ (%age%)" dic
                `shouldBe` Right "%school% 太郎 (22)"
        it "checks fill in a invalid case" $
            fill "$school$ %name%" dic
                `shouldNotBe` Right ""
        it "checks fill in another invalid case" $
            fill "100% $name" dic
                `shouldNotBe` Right ""
    describe "fill' function" $ do
        let config = Config { reqFrom = "<<"
                            , reqTo   = ">>"
                            , optFrom = "#{"
                            , optTo   = "}"
                            }
        it "checks fill' in a simple case" $
            fill' config "<<name>>さん" dic
                `shouldBe` Right "太郎さん"
        it "checks fill' in another simple case" $
            fill' config "#{school} <<name>> (#{age})" dic
                `shouldBe` Right "#{school} 太郎 (22)"
        it "checks fill' in a invalid case" $
            fill' config "<<school>> #{name}" dic
                `shouldNotBe` Right ""
        it "checks fill' in another invalid case" $
            fill' config "100% <<name" dic
                `shouldNotBe` Right ""
