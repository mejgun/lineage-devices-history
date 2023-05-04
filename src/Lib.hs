{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import BuildTargets qualified
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Devices qualified
import System.Directory qualified as D
import System.Process qualified as P

someFunc :: IO ()
someFunc = do
  D.setCurrentDirectory "hudson"
  _ <- P.readProcess "git" ["stash"] ""
  _ <- P.readProcessWithExitCode "git" ["stash", "drop"] ""
  _ <- P.readProcess "git" ["checkout", "master"] ""
  devices <- Devices.read "updater/devices.json"
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
  mapM_ parseBuildFile l
  putStrLn ""
  _ <- P.readProcess "git" ["checkout", "master"] ""
  print $ head devices

parseLogEntry :: String -> (String, UTCTime)
parseLogEntry s = case words s of
  [x1, x2] -> (x1, t)
    where
      t = posixSecondsToUTCTime $ fromIntegral (read x2 :: Int)
  _ -> error $ "bad log format: " ++ s

parseBuildFile :: (String, b) -> IO ()
parseBuildFile (cmt, _) = do
  _ <- P.readProcess "git" ["checkout", "--quiet", cmt] ""
  a <- D.listDirectory "."
  files <- mapM BS.readFile $ fltr a
  let res = BuildTargets.targetParse <$> concat (B8.lines <$> files)
  mapM_ print res
  where
    isBF = BS.isSuffixOf "-build-targets"
    fltr = filter (isBF . B8.pack)
