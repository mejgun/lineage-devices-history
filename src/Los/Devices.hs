{-# LANGUAGE OverloadedStrings #-}

module Los.Devices
  ( Los.Devices.update,
    Los.Devices.init,
  )
where

import Control.Monad (filterM)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Either (rights)
import System.Directory (doesFileExist)
import Types qualified

paths :: [FilePath]
paths =
  [ "updater/devices.json",
    "getcm-devices/devices.json"
  ]

initPath :: FilePath
initPath = "devices.json"

init :: IO Types.DeviceMap
init = readMaps [initPath]

update :: Types.DeviceMap -> IO Types.DeviceMap
update m = Types.concatDevicemaps <$> readMaps paths <*> pure m

readMaps :: [FilePath] -> IO Types.DeviceMap
readMaps files = Types.devicelistToMap <$> readFiles files

readFiles :: [FilePath] -> IO [Types.Device]
readFiles files = ps >>= r >>= s
  where
    ps :: IO [FilePath]
    ps = filterM doesFileExist files

    r :: [FilePath] -> IO [IO (Either String [Types.Device])]
    r = pure . fmap eitherDecodeFileStrict

    s :: [IO (Either String [Types.Device])] -> IO [Types.Device]
    s x = concat . rights <$> sequence x