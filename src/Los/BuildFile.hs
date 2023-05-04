{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Los.BuildFile (Los.BuildFile.read) where

import BuildTargets qualified
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Maybe (catMaybes)
import System.Directory qualified as D

read :: IO [BuildTargets.Target]
read = do
  a <- D.listDirectory "."
  files <- mapM BS.readFile $ fltr a
  let res = BuildTargets.targetParse <$> concat (B8.lines <$> files)
  pure $ catMaybes res
  where
    isBF = BS.isSuffixOf "-build-targets"
    fltr = filter (isBF . B8.pack)
