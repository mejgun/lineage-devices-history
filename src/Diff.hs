module Diff (get, Action (..), Info, sort) where

import Data.HashMap.Strict qualified as HM
import Data.List (sortBy)
import Data.Text qualified as T
import Types qualified

type Info = (Types.Model, Action)

data Action
  = Added [Types.Branch]
  | Switched [Types.Branch] [Types.Branch]
  | Removed [Types.Branch]
  deriving (Eq, Show)

get :: Types.TargetMap -> Types.TargetMap -> [Info]
get prev new = added ++ switched ++ removed
  where
    p = removeSameBranches prev new
    n = removeSameBranches new prev
    added = getAdded p n
    removed = getRemoved p n
    switched = getSwitched p n

    getAdded :: Types.TargetMap -> Types.TargetMap -> [Info]
    getAdded (Types.TargetMap pr) (Types.TargetMap nw) =
      map (\(k, v) -> (k, Added v)) . HM.toList $ HM.difference nw pr

    getRemoved :: Types.TargetMap -> Types.TargetMap -> [Info]
    getRemoved (Types.TargetMap prv) (Types.TargetMap nw) =
      map (\(k, v) -> (k, Removed v)) . HM.toList $ HM.difference prv nw

    getSwitched :: Types.TargetMap -> Types.TargetMap -> [Info]
    getSwitched (Types.TargetMap prv) (Types.TargetMap nw) =
      map (\(mdl, (fromBrnchs, toBrnchs)) -> (mdl, Switched fromBrnchs toBrnchs))
        . HM.toList
        $ HM.intersectionWith (,) prv nw

sort :: Types.DeviceMap -> [Info] -> [Info]
sort (Types.DeviceMap dm) = sortBy (\(a, _) (b, _) -> cmp (go a) (go b))
  where
    go :: Types.Model -> (T.Text, T.Text)
    go x = case HM.lookup x dm of
      Nothing -> error $ show x
      Just (Types.OEM o, Types.Name n) -> (o, n)

    cmp (a1, b1) (a2, b2) = case compare a1 a2 of
      EQ -> compare b1 b2
      x -> x

removeSameBranches :: Types.TargetMap -> Types.TargetMap -> Types.TargetMap
removeSameBranches (Types.TargetMap m1) (Types.TargetMap m2) =
  Types.TargetMap $ HM.filter (not . null) $ HM.mapWithKey fix m1
  where
    fix k v = case HM.lookup k m2 of
      Nothing -> v
      Just v2 -> filter (`notElem` v2) v
