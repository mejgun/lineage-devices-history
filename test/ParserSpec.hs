{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Control.Exception (evaluate)
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HM
import Los.BuildFile.Parser qualified
import Test.Hspec
  ( anyException,
    context,
    describe,
    hspec,
    it,
    shouldBe,
    shouldThrow,
  )
import Test.QuickCheck ()
import Types qualified

testInput :: [BS.ByteString]
testInput =
  [ "",
    "# Weekly 20.0",
    "cm_a52q-userdebug cm-10.0 W",
    "cyanogen_a82q-eng lineage-21.0 W",
    "cm_a62q cm-11.0 W",
    "a72q userdebug lineage-20.0 W",
    "a72q userdebug lineage-22.0",
    "a72q userdebug lineage-23.0 W",
    "#anothercomment",
    "",
    ""
  ]

testOutput :: Types.TargetMap
testOutput =
  Types.TargetMap
    ( HM.fromList
        [ (Types.Model "a52q", [Types.Branch "cm-10.0"]),
          (Types.Model "a82q", [Types.Branch "lineage-21.0"]),
          (Types.Model "a62q", [Types.Branch "cm-11.0"]),
          ( Types.Model "a72q",
            [ Types.Branch "lineage-20.0",
              Types.Branch "lineage-22.0",
              Types.Branch "lineage-23.0"
            ]
          )
        ]
    )

main :: IO ()
main = hspec $ do
  describe "Los.BuildFile.Parser.parseLines" $ do
    context "if argument list is empty" $ do
      it "returns empty map" $ do
        Los.BuildFile.Parser.parseLines [] `shouldBe` Types.TargetMap HM.empty

    context "if argument is valid build files content" $ do
      it "returns valid target map" $ do
        Los.BuildFile.Parser.parseLines testInput `shouldBe` testOutput

    context "if argument is not build file" $ do
      it "throws an error" $ do
        evaluate (Los.BuildFile.Parser.parseLines ["junk"]) `shouldThrow` anyException
