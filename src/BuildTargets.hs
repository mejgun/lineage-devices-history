{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module BuildTargets (Target (..), targetParse) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Attoparsec.ByteString qualified as P
import Data.Attoparsec.ByteString.Char8 qualified as P8
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Either (isRight)
import Data.Functor (($>))

targetParse :: BS.ByteString -> Maybe Target
targetParse x | isRight (P.parseOnly commentParser x) = Nothing
targetParse x | isRight (P.parseOnly emptyParser x) = Nothing
targetParse x = case P.parseOnly targetParser x of
  Right r -> Just r
  Left s -> error $ s ++ "cannot parse " ++ B8.unpack x

data Comment = Comment

commentParser :: P.Parser Comment
commentParser = (P.many' P8.space *> P.string "#") $> Comment

data Empty = Empty

emptyParser :: P.Parser Empty
emptyParser = (P.many' P8.space *> P.endOfInput) $> Empty

type Model = String

type Branch = BS.ByteString

data Target = Target Model Branch deriving (Show)

targetParser :: P.Parser Target
targetParser =
  P.many' P8.space
    *> ( cmParser
           <|> cmNoUsrDbgParser
           <|> cyanParser
           <|> losParser
       )
  where
    cmParser :: P.Parser Target
    cmParser = do
      _ <- P.string "cm_"
      m <- P.manyTill P8.anyChar "-userdebug "
      guard $ not . null $ m
      target <- P.takeWhile1 $ P.notInClass " \r\n"
      return $ Target m target

    cmNoUsrDbgParser :: P.Parser Target
    cmNoUsrDbgParser = do
      _ <- P.string "cm_"
      m <- P.manyTill P8.anyChar " "
      guard $ not . null $ m
      target <- P.takeWhile1 $ P.notInClass " \r\n"
      return $ Target m target

    cyanParser :: P.Parser Target
    cyanParser = do
      _ <- P.string "cyanogen_"
      m <- P.manyTill P8.anyChar "-eng "
      guard $ not . null $ m
      target <- P.takeWhile1 $ P.notInClass " \r\n"
      return $ Target m target

    losParser :: P.Parser Target
    losParser = do
      m <- P.manyTill P8.anyChar " userdebug "
      guard $ not . null $ m
      target <- P.takeWhile1 $ P.notInClass " \r\n"
      return $ Target m target
