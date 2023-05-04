{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Git
  ( listCommits,
    Commit,
    openRepo,
    checkout,
  )
where

import Control.Monad (void)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory qualified as D
import System.Process qualified as P

type Commit = (String, UTCTime)

openRepo :: FilePath -> IO ()
openRepo path =
  D.setCurrentDirectory path
    >> P.readProcess "git" ["stash"] ""
    >> P.readProcessWithExitCode "git" ["stash", "drop"] ""
    >> checkout "master"

checkout :: String -> IO ()
checkout commit = void $ P.readProcess "git" ["checkout", "--quiet", commit] ""

listCommits :: IO [Commit]
listCommits = p >>= l >>= e
  where
    p :: IO String
    p = P.readProcess "git" ["log", "--pretty=format:%H %ct"] ""

    l :: String -> IO [String]
    l = pure . reverse . lines

    e :: [String] -> IO [Commit]
    e = pure . fmap parseLogEntry

parseLogEntry :: String -> Commit
parseLogEntry s = case words s of
  [x1, x2] -> (x1, t)
    where
      t = posixSecondsToUTCTime $ fromIntegral (read x2 :: Int)
  _ -> error $ "bad log format: " ++ s
