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
brnchsAdded = branchsToTags "is-success"

brnchsRemoved :: [Types.Branch] -> H.Html
brnchsRemoved = branchsToTags "is-danger"

brnchsSwitched :: [Types.Branch] -> H.Html
brnchsSwitched = branchsToTags "is-info"

branchsToTags :: T.Text -> [Types.Branch] -> H.Html
branchsToTags t =
  sequence_
    . List.intersperse (", " :: H.Html)
    . map (\(Types.Branch b) -> H.span (H.toHtml b) H.! HA.class_ cl)
  where
    cl = H.textValue $ T.intercalate " " ["tag", "is-medium", t]

modelToHtml :: Types.Model -> H.Html
modelToHtml (Types.Model mdl) = H.td $ H.toHtml mdl

nameToHtml :: Types.DeviceMap -> Types.Model -> H.Html
nameToHtml (Types.DeviceMap devices) mdl@(Types.Model m) =
  case HM.lookup mdl devices of
    Just (Types.OEM o, Types.Name n) ->
      H.td $ do
        H.toHtml o
        " "
        H.a H.! HA.href (H.textValue (T.concat [m, ".html"])) $ H.toHtml n
    _ -> error "device not found, could not be"
