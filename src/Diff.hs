{-# LANGUAGE ImportQualifiedPost #-}

module Diff (printDelta) where

import Data.Maybe (isNothing, mapMaybe)
import Git qualified
import Los.BuildFile.Parser qualified as P
import Los.Devices qualified

printDelta ::
  Los.Devices.DeviceMap ->
  (Git.Commit, [P.Target]) ->
  (Git.Commit, [P.Target]) ->
  IO (Git.Commit, [P.Target])
printDelta _devices _prevCommit@(_, od) newCommit@((_, dt), nd) = do
  print dt
  mapM_ print $ mapMaybe (getAction od) nd
  mapM_ print (Removed <$> getRemoved od nd)
  pure newCommit

--   let a = filter (\(P.Target m _) -> not (HM.member m devices)) d
--    in mapM_ print a >> pure newCommit

data Action
  = Added P.Target
  | Switched P.Target P.Target
  | Removed P.Target
  deriving (Show)

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
