{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Los.Devices
  ( Los.Devices.update,
    Los.Devices.init,
    DeviceMap,
    Item (..),
  )
where

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
import Data.Text qualified as T
import System.Directory (doesFileExist)

paths :: [FilePath]
paths =
  [ "updater/devices.json",
    "getcm-devices/devices.json"
  ]

initPath :: FilePath
initPath = "devices.json"

data Item = Item
  { model :: T.Text,
    oem :: T.Text,
    name :: T.Text
  }
  deriving (Show)

instance FromJSON Item where
  parseJSON = withObject "Item" $ \o -> do
    model <- o .: "model"
    oem <- o .: "oem" <|> o .: "brand"
    name <- o .: "name"
    return Item {..}

type DeviceMap = HM.HashMap T.Text Item

init :: IO DeviceMap
init = readMaps [initPath]

update :: DeviceMap -> IO DeviceMap
update m = HM.union <$> readMaps paths <*> pure m

readMaps :: [FilePath] -> IO DeviceMap
readMaps files = HM.fromList . toEntryList <$> readFiles files
  where
    toEntryList = fmap (\e -> (model e, e))

readFiles :: [FilePath] -> IO [Item]
readFiles files = ps >>= r >>= s
  where
    ps :: IO [FilePath]
    ps = filterM doesFileExist files

    r :: [FilePath] -> IO [IO (Either String [Item])]
    r = pure . fmap eitherDecodeFileStrict

    s :: [IO (Either String [Item])] -> IO [Item]
    s x = concat . rights <$> sequence x