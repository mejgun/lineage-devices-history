{-# LANGUAGE ImportQualifiedPost #-}

module Diff (get, Diffs, Action (..)) where

import Data.Maybe (isNothing, mapMaybe)
import Git qualified
import Los.BuildFile.Parser qualified as P

data Action
  = Added P.Target
  | Switched P.Target P.Target
  | Removed P.Target
  deriving (Show)

type Diffs = (Git.Commit, [Action])

get :: (Git.Commit, [P.Target]) -> (Git.Commit, [P.Target]) -> Diffs
get _prevCommit@(_, od) _newCommit@(cmt, nd) =
  (cmt, addswitch ++ remov)
  where
    addswitch = mapMaybe (getAction od) nd
    remov = Removed <$> getRemoved od nd

getRemoved :: [P.Target] -> [P.Target] -> [P.Target]
getRemoved xs ys = filter (isNothing . isExist ys) xs

getAction :: [P.Target] -> P.Target -> Maybe Action
getAction xs y = do
  case isExist xs y of
    Nothing -> Just $ Added y
    Just x -> isSwitched x y
      where
        isSwitched (P.Target _ ot) (P.Target _ nt)
          | ot == nt = Nothing
          | otherwise = Just $ Switched x y

isExist :: [P.Target] -> P.Target -> Maybe P.Target
isExist xs (P.Target ym _) = case filter (\(P.Target xm _) -> xm == ym) xs of
  [x] -> Just x
  _ -> Nothing
