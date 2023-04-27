{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Devices (Devices.read) where

import Data.Aeson
import Data.Text (Text)

data Item = Item
  { model :: Text,
    oem :: Text,
    name :: Text
  }
  deriving (Show)

instance FromJSON Item where
  parseJSON = withObject "Item" $ \o -> do
    model <- o .: "model"
    oem <- o .: "oem"
    name <- o .: "name"
    return Item {..}

read :: FilePath -> IO [Item]
read filename = do
  result <- eitherDecodeFileStrict filename :: IO (Either String [Item])
  case result of
    Left err -> error err
    Right items -> pure items