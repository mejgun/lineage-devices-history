module Diff (get, Action (..), Info) where

import Data.HashMap.Strict qualified as HM
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

removeSameBranches :: Types.TargetMap -> Types.TargetMap -> Types.TargetMap
removeSameBranches (Types.TargetMap m1) (Types.TargetMap m2) =
  Types.TargetMap $ HM.filter (not . null) $ HM.mapWithKey fix m1
  where
    fix k v = case HM.lookup k m2 of
      Nothing -> v
      Just v2 -> filter (`notElem` v2) v

getAdded :: Types.TargetMap -> Types.TargetMap -> [Info]
getAdded (Types.TargetMap prev) (Types.TargetMap new) =
  map (\(k, v) -> (k, Added v)) . HM.toList $ HM.difference new prev

getRemoved :: Types.TargetMap -> Types.TargetMap -> [Info]
getRemoved (Types.TargetMap prev) (Types.TargetMap new) =
  map (\(k, v) -> (k, Removed v)) . HM.toList $ HM.difference prev new

getSwitched :: Types.TargetMap -> Types.TargetMap -> [Info]
getSwitched (Types.TargetMap prev) (Types.TargetMap new) =
  map (\(mdl, (fromBrnchs, toBrnchs)) -> (mdl, Switched fromBrnchs toBrnchs))
    . HM.toList
    $ HM.intersectionWith (,) prev new
