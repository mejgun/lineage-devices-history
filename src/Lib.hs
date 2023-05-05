{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib (Lib.start) where

import Control.Monad (foldM, foldM_)
import Data.HashMap.Strict qualified as HM
import Data.List (sortOn)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Diff qualified
import Git qualified
import Los.BuildFile qualified
import Los.BuildFile.Parser qualified
import Los.Devices qualified

type CommitMap = HM.HashMap Git.Commit [Los.BuildFile.Parser.Target]

type Acc = (Los.Devices.DeviceMap, CommitMap)

start :: IO ()
start = do
  initDevices <- Los.Devices.init
  Git.openRepo "hudson"
  l <- Git.listCommits
  (res1, res2) <- foldM handleCommit (initDevices, HM.empty) l
  Git.checkout "master"
  let commits = sortOn (snd . fst) $ filter (not . null . snd) $ HM.toList res2
  -- mapM_ print $ sortOn fst $ HM.toList res1
  -- mapM_ print commits
  foldM_ (Diff.printDelta res1) (("", posixSecondsToUTCTime 1), []) commits
  putStrLn "done"

handleCommit :: Acc -> Git.Commit -> IO Acc
handleCommit (devs, commits) c@(cmt, _) = do
  Git.checkout cmt
  newdevs <- Los.Devices.update devs
  ts <- Los.BuildFile.read
  pure (newdevs, HM.insert c ts commits)
