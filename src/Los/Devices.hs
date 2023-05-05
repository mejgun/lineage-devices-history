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
import Data.Text (Text)
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

init :: IO DeviceMap
init = readMaps [initPath]

update :: DeviceMap -> IO DeviceMap
update m = HM.union <$> readMaps paths <*> pure m

readMaps :: [FilePath] -> IO DeviceMap
readMaps files = do
  HM.fromList . toEntryList <$> readFiles files
  where
    toEntryList = fmap (\e -> (T.unpack (model e), e))

readFiles :: [FilePath] -> IO [Item]
readFiles files = s r
  where
    ps :: IO [FilePath]
    ps = filterM doesFileExist files
    r :: IO [IO (Either String [Item])]
    r = fmap eitherDecodeFileStrict <$> ps
    s :: IO [IO (Either String [Item])] -> IO [Item]
    s x = do
      y <- sequence <$> x
      concat . rights <$> y