{-# LANGUAGE OverloadedStrings #-}

module Lib (Lib.start) where

import Control.Monad (foldM)
import Data.HashMap.Strict qualified as HM
import Data.List (sortOn)
import Diff qualified
import Git qualified
import Html (saveDiffs)
import Los.BuildFile qualified
import Los.Devices qualified
import Types qualified

type CommitMap = HM.HashMap Git.Commit Types.TargetMap

start :: IO ()
start = do
  initDevices <- Los.Devices.init
  Git.openRepo "hudson"
  l <- Git.listCommits
  (res1, res2) <- foldM handleCommit (initDevices, HM.empty) l
  Git.leaveRepo
  let commits =
        sortOn (snd . fst) $
          filter (\(_, Types.TargetMap x) -> not (HM.null x)) $
            HM.toList res2
  -- mapM_ print $ sortOn fst $ HM.toList res1
  -- Diff.get res1
  let (diffs, _) = foldl getDelta ([], Types.TargetMap HM.empty) commits
  saveDiffs res1 $ filter (not . null . snd) diffs
  putStrLn "done"

type Acc = (Types.DeviceMap, CommitMap)

handleCommit :: Acc -> Git.Commit -> IO Acc
handleCommit (devs, commits) c@(cmt, _) = do
  Git.checkout cmt
  newdevs <- Los.Devices.update devs
  ts <- Los.BuildFile.read
  pure (newdevs, HM.insert c ts commits)

type Acc2 = ([(Git.Commit, [Diff.Action])], Types.TargetMap)

getDelta :: Acc2 -> (Git.Commit, Types.TargetMap) -> Acc2
getDelta _acc@(deltas, oldcommit) _commit@(c, m) =
  let d = Diff.get oldcommit m
   in (deltas ++ [(c, d)], m)
