{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Devices qualified
import System.Directory qualified as D
import System.Process qualified as P

someFunc :: IO ()
someFunc = do
  D.setCurrentDirectory "hudson"
  _ <- P.readProcess "git" ["checkout", "master"] ""
  s <- P.readProcess "git" ["log", "--pretty=format:%H %ct"] ""
  let l = reverse $ parseLogEntry <$> lines s
  mapM_ (\e -> P.readProcess "git" ["checkout", fst e] "") l
  _ <- P.readProcess "git" ["checkout", "master"] ""
  Devices.read "updater/devices.json" >>= mapM_ print

parseLogEntry :: String -> (String, UTCTime)
parseLogEntry s = case words s of
  [x1, x2] -> (x1, t)
    where
      t = posixSecondsToUTCTime $ fromIntegral (read x2 :: Int)
  _ -> error $ "bad log format: " ++ s