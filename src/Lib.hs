{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import BuildTargets qualified
import Control.Monad (foldM)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.HashMap.Strict qualified as HM
import Data.Maybe (catMaybes)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Devices (DeviceMap)
import Devices qualified
import System.Directory qualified as D
import System.Process qualified as P

type Commit = (String, UTCTime)

type CommitMap = HM.HashMap Commit [BuildTargets.Target]

someFunc :: IO ()
someFunc = do
  D.setCurrentDirectory "hudson"
  _ <- P.readProcess "git" ["stash"] ""
  _ <- P.readProcessWithExitCode "git" ["stash", "drop"] ""
  _ <- P.readProcess "git" ["checkout", "master"] ""
  -- devices <- Devices.read "updater/devices.json"
  s <- P.readProcess "git" ["log", "--pretty=format:%H %ct"] ""
  let l = reverse $ parseLogEntry <$> lines s
  {-
  mapM_
    ( \e ->
        print e
          >> P.readProcess "git" ["checkout", "--quiet", fst e] ""
          >> D.listDirectory "."
          >>= print
    )
    l
  -}
  (res1, res2) <- foldM handleCommit (HM.empty, HM.empty) l
  putStrLn ""
  _ <- P.readProcess "git" ["checkout", "master"] ""
  putStrLn "done"
  mapM_ print $ HM.toList res1
  mapM_ print $ HM.toList res2

-- print $ head devices

parseLogEntry :: String -> Commit
parseLogEntry s = case words s of
  [x1, x2] -> (x1, t)
    where
      t = posixSecondsToUTCTime $ fromIntegral (read x2 :: Int)
  _ -> error $ "bad log format: " ++ s

handleCommit ::
  (Devices.DeviceMap, CommitMap) ->
  (String, UTCTime) ->
  IO (DeviceMap, CommitMap)
handleCommit (devs, commits) c@(cmt, _) = do
  _ <- P.readProcess "git" ["checkout", "--quiet", cmt] ""
  newdevs <- Devices.update devs
  ts <- parseBuildFile
  pure (newdevs, HM.insert c ts commits)

parseBuildFile :: IO [BuildTargets.Target]
parseBuildFile = do
  a <- D.listDirectory "."
  files <- mapM BS.readFile $ fltr a
  let res = BuildTargets.targetParse <$> concat (B8.lines <$> files)
  pure $ catMaybes res
  where
    isBF = BS.isSuffixOf "-build-targets"
    fltr = filter (isBF . B8.pack)
