{-# LANGUAGE OverloadedStrings #-}

module SortSpec where

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

map1 :: Types.DeviceMap
map1 =
  Types.DeviceMap
    ( HM.fromList
        [ (Types.Model "a52", (Types.OEM "Sony", Types.Name "Alpha")),
          (Types.Model "b52", (Types.OEM "Samsung", Types.Name "Galaxy S1")),
          (Types.Model "c52", (Types.OEM "Samsung", Types.Name "Galaxy S5")),
          (Types.Model "d52", (Types.OEM "Google", Types.Name "Pixel 9"))
        ]
    )

actions :: [Diff.Info]
actions =
  [ (Types.Model "a52", Diff.Added [Types.Branch "cm-12.0"]),
    (Types.Model "b52", Diff.Switched [Types.Branch "cm-10.0"] [Types.Branch "cm-11.0"]),
    (Types.Model "c52", Diff.Removed [Types.Branch "lineage-21.0"]),
    (Types.Model "d52", Diff.Removed [Types.Branch "lineage-22.0"])
  ]

sortedActions :: [Diff.Info]
sortedActions =
  [ (Types.Model "d52", Diff.Removed [Types.Branch "lineage-22.0"]),
    (Types.Model "b52", Diff.Switched [Types.Branch "cm-10.0"] [Types.Branch "cm-11.0"]),
    (Types.Model "c52", Diff.Removed [Types.Branch "lineage-21.0"]),
    (Types.Model "a52", Diff.Added [Types.Branch "cm-12.0"])
  ]

main :: IO ()
main = hspec $ do
  describe "Diff.sort" $ do
    context "" $ do
      it "" $ do
        Diff.sort map1 actions `shouldBe` sortedActions
