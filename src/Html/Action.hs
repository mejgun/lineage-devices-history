{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Html.Action (toHtml) where

import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Text qualified as T
import Diff qualified
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as HA
import Types qualified

toHtml :: Types.DeviceMap -> Diff.Action -> H.Html
toHtml devices (Diff.Added mdl brnch) = do
  H.td $ do
    brnchsAdded brnch
  nameToHtml devices mdl
  modelToHtml mdl
toHtml devices (Diff.Removed mdl brnch) = do
  H.td $ do
    brnchsRemoved brnch
  nameToHtml devices mdl
  modelToHtml mdl
toHtml devices (Diff.Switched mdl brnch1 brnch2) = do
  H.td $ do
    brnchsSwitched brnch1
    " → "
    brnchsSwitched brnch2
  nameToHtml devices mdl
  modelToHtml mdl

brnchsAdded :: [Types.Branch] -> H.Html
brnchsAdded = branchsToTags "tag is-medium is-success"

brnchsRemoved :: [Types.Branch] -> H.Html
brnchsRemoved = branchsToTags "tag is-medium is-danger"

brnchsSwitched :: [Types.Branch] -> H.Html
brnchsSwitched = branchsToTags "tag is-medium is-info"

branchsToTags :: T.Text -> [Types.Branch] -> H.Html
branchsToTags t =
  mapM_ H.toHtml
    . List.intersperse (", " :: H.Html)
    . map (\(Types.Branch b) -> H.span (H.toHtml b) H.! HA.class_ (H.textValue t))

modelToHtml :: Types.Model -> H.Html
modelToHtml (Types.Model mdl) = H.td $ H.toHtml mdl

nameToHtml :: Types.DeviceMap -> Types.Model -> H.Html
nameToHtml (Types.DeviceMap devices) mdl =
  case HM.lookup mdl devices of
    Just (Types.OEM o, Types.Name n) -> H.td $ H.toHtml $ T.intercalate " " [o, n]
    _ -> error "device not found, could not be"
