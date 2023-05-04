{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib (Lib.start) where

import Control.Monad (foldM, foldM_)
import Data.HashMap.Strict qualified as HM
import Data.List (sortOn)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Git qualified
import Los.BuildFile qualified
import Los.BuildFile.Parser qualified
import Los.Devices qualified
import System.Process qualified as P

type CommitMap = HM.HashMap Git.Commit [Los.BuildFile.Parser.Target]

type Acc = (Los.Devices.DeviceMap, CommitMap)

start :: IO ()
start = do
  Git.openRepo "hudson"
  l <- Git.listCommits
  (res1, res2) <- foldM handleCommit (HM.empty, HM.empty) l
  putStrLn ""
  _ <- P.readProcess "git" ["checkout", "master"] ""
  let commits = sortOn (snd . fst) $ filter (not . null . snd) $ HM.toList res2
  -- mapM_ print $ sortOn fst $ HM.toList res1
  -- mapM_ print commits
  foldM_ (printIncrement res1) (("", posixSecondsToUTCTime 1), []) commits
  putStrLn "done"

handleCommit :: Acc -> Git.Commit -> IO Acc
handleCommit (devs, commits) c@(cmt, _) = do
  Git.checkout cmt
  newdevs <- Los.Devices.update devs
  ts <- Los.BuildFile.read
  pure (newdevs, HM.insert c ts commits)

printIncrement ::
  Los.Devices.DeviceMap ->
  (Git.Commit, [Los.BuildFile.Parser.Target]) ->
  (Git.Commit, [Los.BuildFile.Parser.Target]) ->
  IO (Git.Commit, [Los.BuildFile.Parser.Target])
printIncrement devices _prevCommit (newCommit@(_, d)) = do
  let a = filter (\(Los.BuildFile.Parser.Target m _) -> not (HM.member m devices)) d
   in mapM_ print a >> pure newCommit