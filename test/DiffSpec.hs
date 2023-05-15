{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module DiffSpec where

import Data.HashMap.Strict qualified as HM
import Diff qualified
import Test.Hspec
  ( context,
    describe,
    hspec,
    it,
    shouldBe,
  )
import Test.QuickCheck ()
import Types qualified

map1 :: Types.TargetMap
map1 =
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

map2 :: Types.TargetMap
map2 =
  Types.TargetMap
    ( HM.fromList
        [ (Types.Model "a52q", [Types.Branch "cm-11.0"]),
          (Types.Model "a62q", [Types.Branch "cm-11.0", Types.Branch "cm-12.0"]),
          ( Types.Model "a72q",
            [ Types.Branch "lineage-20.0",
              Types.Branch "lineage-23.0"
            ]
          )
        ]
    )

actions :: [Diff.Action]
actions =
  [ Diff.Added (Types.Model "a62q") [Types.Branch "cm-12.0"],
    Diff.Switched (Types.Model "a52q") [Types.Branch "cm-10.0"] [Types.Branch "cm-11.0"],
    Diff.Removed (Types.Model "a82q") [Types.Branch "lineage-21.0"],
    Diff.Removed (Types.Model "a72q") [Types.Branch "lineage-22.0"]
  ]

main :: IO ()
main = hspec $ do
  describe "Diff.get" $ do
    context "if arguments are two targetmaps" $ do
      it "returns valid actions list" $ do
        Diff.get map1 map2 `shouldBe` actions

    context "if arguments are empty maps" $ do
      it "return empty actions list" $ do
        Diff.get (Types.TargetMap HM.empty) (Types.TargetMap HM.empty) `shouldBe` []
