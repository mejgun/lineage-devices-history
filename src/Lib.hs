{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib (Lib.start) where

import Control.Monad (foldM)
import Data.HashMap.Strict qualified as HM
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
  putStrLn "done"
  mapM_ print $ HM.toList res1
  mapM_ print $ HM.toList res2

handleCommit :: Acc -> Git.Commit -> IO Acc
handleCommit (devs, commits) c@(cmt, _) = do
  Git.checkout cmt
  newdevs <- Los.Devices.update devs
  ts <- Los.BuildFile.read
  pure (newdevs, HM.insert c ts commits)
