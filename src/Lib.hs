{-# LANGUAGE OverloadedStrings #-}

module Lib (Lib.start) where

import Control.Monad (foldM)
import Data.HashMap.Strict qualified as HM
import Diff qualified
import Git qualified
import Html (saveDiffs)
import Los.BuildFile qualified
import Los.Devices qualified
import Types qualified

type CommitsList = [(Git.Commit, Types.TargetMap)]

start :: IO ()
start = do
  initDevices <- Los.Devices.init
  Git.openRepo "hudson"
  l <- Git.listCommits
  (res1, res2) <- foldM handleCommit (initDevices, []) l
  Git.leaveRepo
  let commits = reverse $ filter (\(_, Types.TargetMap x) -> not (HM.null x)) res2
      (diffs_t, _) = foldl getDelta ([], Types.TargetMap HM.empty) commits
      diffs = map (\(a, b) -> (a, Diff.sort res1 b)) diffs_t
  saveDiffs res1 $ filter (not . null . snd) diffs
  putStrLn "done"

type Acc = (Types.DeviceMap, CommitsList)

handleCommit :: Acc -> Git.Commit -> IO Acc
handleCommit (devs, commits) c@(Git.Commit cmt _) = do
  Git.checkout cmt
  newdevs <- Los.Devices.update devs
  ts <- Los.BuildFile.read
  pure (newdevs, (c, ts) : commits)

type Acc2 = ([(Git.Commit, [Diff.Info])], Types.TargetMap)

getDelta :: Acc2 -> (Git.Commit, Types.TargetMap) -> Acc2
getDelta _acc@(deltas, oldcommit) _commit@(c, m) =
  let d = Diff.get oldcommit m
   in (deltas ++ [(c, d)], m)
