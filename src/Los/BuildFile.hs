{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Los.BuildFile (Los.BuildFile.read) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Los.BuildFile.Parser qualified
import System.Directory qualified as D
import Types qualified

read :: IO Types.TargetMap
read = do
  a <- D.listDirectory "."
  files <- mapM BS.readFile $ fltr a
  let res =
        Los.BuildFile.Parser.parseLines
          . B8.lines
          $ BS.intercalate "\n\n\n" files
   in pure res
  where
    isBF = BS.isSuffixOf "-build-targets"
    fltr = filter (isBF . B8.pack)
