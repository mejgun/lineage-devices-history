{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Los.BuildFile (Los.BuildFile.read) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Maybe (catMaybes)
import Los.BuildFile.Parser qualified
import System.Directory qualified as D

read :: IO [Los.BuildFile.Parser.Target]
read = do
  a <- D.listDirectory "."
  files <- mapM BS.readFile $ fltr a
  let res = Los.BuildFile.Parser.parseLine <$> concat (B8.lines <$> files)
  pure $ catMaybes res
  where
    isBF = BS.isSuffixOf "-build-targets"
    fltr = filter (isBF . B8.pack)
