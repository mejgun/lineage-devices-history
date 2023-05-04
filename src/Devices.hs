{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Devices (Devices.update, DeviceMap, Item (..)) where

import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Data.Aeson
  ( FromJSON (parseJSON),
    eitherDecodeFileStrict,
    withObject,
    (.:),
  )
import Data.Either (rights)
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (doesFileExist)

paths :: [FilePath]
paths =
  [ "updater/devices.json",
    "getcm-devices/devices.json"
  ]

data Item = Item
  { model :: Text,
    oem :: Text,
    name :: Text
  }
  deriving (Show)

instance FromJSON Item where
  parseJSON = withObject "Item" $ \o -> do
    model <- o .: "model"
    oem <- o .: "oem" <|> o .: "brand"
    name <- o .: "name"
    return Item {..}

type DeviceMap = HM.HashMap String Item

update :: DeviceMap -> IO DeviceMap
update m = do
  r <- readFiles
  let tempMap = HM.fromList $ fmap (\e -> (T.unpack (model e), e)) r
  pure $ HM.union tempMap m

readFiles :: IO [Item]
readFiles = s r
  where
    ps :: IO [FilePath]
    ps = filterM doesFileExist paths
    r :: IO [IO (Either String [Item])]
    r = fmap eitherDecodeFileStrict <$> ps
    s :: IO [IO (Either String [Item])] -> IO [Item]
    s x = do
      y <- sequence <$> x
      concat . rights <$> y

-- read :: FilePath -> IO [Item]
-- read filename = do
--   result <- eitherDecodeFileStrict filename :: IO (Either String [Item])
--   case result of
--     Left err -> error err
--     Right items -> pure items