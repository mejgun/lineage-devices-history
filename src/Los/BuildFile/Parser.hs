{-# LANGUAGE OverloadedStrings #-}

module Los.BuildFile.Parser (parseLines) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Attoparsec.ByteString qualified as P
import Data.Attoparsec.ByteString.Char8 qualified as P8
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Either (isRight)
import Data.Functor (($>))
import Data.HashMap.Strict qualified as HM
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Types qualified

data Target = Target Types.Model [Types.Branch]

parseLines :: [BS.ByteString] -> Types.TargetMap
parseLines xs = toMap $ foldr f [] $ mapMaybe parseLine xs
  where
    toMap = Types.TargetMap . HM.fromList . map (\(Target m b) -> (m, b))

    f y ys = case filter (inlist y) ys of
      [] -> y : ys
      [x] -> concatTargets x y : filter (not . inlist y) ys
      _ -> error "could not be"

    inlist (Target e1 _) (Target e2 _) = e1 == e2

    concatTargets (Target md1 b1) (Target md2 b2)
      | md1 == md2 = Target md1 (nub (b2 ++ b1))
      | otherwise = error "could not be either"

parseLine :: BS.ByteString -> Maybe Target
parseLine x | isRight (P.parseOnly commentParser x) = Nothing
parseLine x | isRight (P.parseOnly emptyParser x) = Nothing
parseLine x = case P.parseOnly targetParser x of
  Right r -> Just r
  Left s -> error $ s ++ "cannot parse " ++ B8.unpack x

data Comment = Comment

commentParser :: P.Parser Comment
commentParser = (P.many' P8.space *> P.string "#") $> Comment

data Empty = Empty

emptyParser :: P.Parser Empty
emptyParser = (P.many' P8.space *> P.endOfInput) $> Empty

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
      m <- getModel P8.anyChar "-userdebug "
      grd m
      target <- TE.decodeUtf8 <$> P.takeWhile1 (P.notInClass " \r\n")
      trgt m target

    cmNoUsrDbgParser :: P.Parser Target
    cmNoUsrDbgParser = do
      _ <- P.string "cm_"
      m <- getModel P8.anyChar " "
      grd m
      target <- TE.decodeUtf8 <$> P.takeWhile1 (P.notInClass " \r\n")
      trgt m target

    cyanParser :: P.Parser Target
    cyanParser = do
      _ <- P.string "cyanogen_"
      m <- getModel P8.anyChar "-eng "
      grd m
      target <- TE.decodeUtf8 <$> P.takeWhile1 (P.notInClass " \r\n")
      trgt m target

    losParser :: P.Parser Target
    losParser = do
      m <- getModel P8.anyChar " userdebug "
      grd m
      target <- TE.decodeUtf8 <$> P.takeWhile1 (P.notInClass " \r\n")
      trgt m target

    trgt m t = return $ Target (Types.Model m) [Types.Branch t]

    getModel x y = T.pack <$> P.manyTill x y

    grd = guard . not . T.null
