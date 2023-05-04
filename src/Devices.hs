{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Devices (Devices.read, Item (..)) where

import Control.Applicative ((<|>))
import Data.Aeson
  ( FromJSON (parseJSON),
    eitherDecodeFileStrict,
    withObject,
    (.:),
  )
import Data.Either (rights)
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)

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

type Devices = HM.HashMap Text Item

updateDevices :: Devices -> IO Devices
updateDevices = undefined

readFiles :: [FilePath] -> IO [Item]
readFiles ps = s r
  where
    r :: [IO (Either String [Item])]
    r = fmap eitherDecodeFileStrict ps
    s :: [IO (Either String [Item])] -> IO [Item]
    s x = do
      y <- sequence x
      let z = rights y
      pure $ concat z

read :: FilePath -> IO [Item]
read filename = do
  result <- eitherDecodeFileStrict filename :: IO (Either String [Item])
  case result of
    Left err -> error err
    Right items -> pure items