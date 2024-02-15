{-# LANGUAGE OverloadedStrings #-}

module Git
  ( listCommits,
    Commit (..),
    openRepo,
    checkout,
    leaveRepo,
  )
where

import Control.Monad (void)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory qualified as D
import System.Process qualified as P

data Commit = Commit T.Text UTCTime deriving (Show)

instance Eq Commit where
  (Commit hash1 _) == (Commit hash2 _) = hash1 == hash2

instance Ord Commit where
  compare (Commit _ t1) (Commit _ t2) = compare t1 t2

instance Hashable Commit where
  hashWithSalt s (Commit h _) = s `hashWithSalt` h

openRepo :: FilePath -> IO ()
openRepo path =
  D.setCurrentDirectory path
    >> P.readProcess "git" ["stash"] ""
    >> P.readProcessWithExitCode "git" ["stash", "drop"] ""
    >> checkout "main"

leaveRepo :: IO ()
leaveRepo = checkout "main" >> D.setCurrentDirectory ".."

checkout :: T.Text -> IO ()
checkout commit = void $ P.readProcess "git" ["checkout", "--quiet", T.unpack commit] ""

listCommits :: IO [Commit]
listCommits = p >>= l >>= e
  where
    p :: IO String
    p = P.readProcess "git" ["log", "--pretty=format:%H %ct"] ""

    l :: String -> IO [T.Text]
    l = pure . reverse . T.lines . T.pack

    e :: [T.Text] -> IO [Commit]
    e = pure . fmap parseLogEntry

parseLogEntry :: T.Text -> Commit
parseLogEntry s = case T.words s of
  [x1, x2] -> Commit x1 t
    where
      t = posixSecondsToUTCTime $ fromIntegral (read (T.unpack x2) :: Int)
  _ -> error $ "bad log format: " ++ T.unpack s
