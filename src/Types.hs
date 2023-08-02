{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Device (..),
    DeviceMap (..),
    Model (..),
    OEM (..),
    Name (..),
    devicelistToMap,
    concatDevicemaps,
    Branch (..),
    TargetMap (..),
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict qualified as HM
import Data.Hashable qualified
import Data.Text qualified as T
import GHC.Generics (Generic)

newtype Model = Model T.Text deriving (Eq, Generic, Show, Ord)

instance Data.Hashable.Hashable Model

newtype OEM = OEM T.Text deriving (Eq, Generic)

instance Data.Hashable.Hashable OEM

newtype Name = Name T.Text

newtype Device = Device (Model, (OEM, Name))

newtype Branch = Branch T.Text
  deriving (Eq, Show)

newtype TargetMap = TargetMap (HM.HashMap Types.Model [Types.Branch])
  deriving (Eq, Show)

newtype DeviceMap = DeviceMap (HM.HashMap Types.Model (Types.OEM, Types.Name))

devicelistToMap :: [Device] -> DeviceMap
devicelistToMap = DeviceMap . HM.fromList . map (\(Device x) -> x)

concatDevicemaps :: DeviceMap -> DeviceMap -> DeviceMap
concatDevicemaps (DeviceMap new) (DeviceMap old) = DeviceMap $ HM.union new old

instance FromJSON Device where
  parseJSON = withObject "Device" $ \o -> do
    model <- o .: "model"
    oem <- o .: "oem" <|> o .: "brand"
    name <- o .: "name"
    return $ Device (Model model, (OEM oem, Name name))

instance FromJSON DeviceMap where
  parseJSON arr =
    devicelistToMap <$> (parseJSON arr :: Parser [Device])
