{-# LANGUAGE ImportQualifiedPost #-}

module Diff (get, Action (..)) where

import Data.HashMap.Strict qualified as HM
import Types qualified

data Action
  = Added Types.Model [Types.Branch]
  | Switched Types.Model [Types.Branch] [Types.Branch]
  | Removed Types.Model [Types.Branch]

get :: Types.TargetMap -> Types.TargetMap -> [Action]
get prev new = added ++ switched ++ removed
  where
    p = removeSameBranches prev new
    n = removeSameBranches new prev
    added = getAdded p n
    removed = getRemoved p n
    switched = getSwitched p n

removeSameBranches :: Types.TargetMap -> Types.TargetMap -> Types.TargetMap
removeSameBranches (Types.TargetMap m1) (Types.TargetMap m2) =
  Types.TargetMap $ HM.filter (not . null) $ HM.mapWithKey fix m1
  where
    fix k v = case HM.lookup k m2 of
      Nothing -> v
      Just v2 -> filter (`notElem` v2) v

getAdded :: Types.TargetMap -> Types.TargetMap -> [Action]
getAdded (Types.TargetMap prev) (Types.TargetMap new) =
  map (uncurry Added) . HM.toList $ HM.difference new prev

getRemoved :: Types.TargetMap -> Types.TargetMap -> [Action]
getRemoved (Types.TargetMap prev) (Types.TargetMap new) =
  map (uncurry Removed) . HM.toList $ HM.difference prev new

getSwitched :: Types.TargetMap -> Types.TargetMap -> [Action]
getSwitched (Types.TargetMap prev) (Types.TargetMap new) =
  map (\(mdl, (fromBrnchs, toBrnchs)) -> Switched mdl fromBrnchs toBrnchs)
    -- . filter (\(_, (x, y)) -> x /= y)
    . HM.toList
    $ HM.intersectionWith (,) prev new
