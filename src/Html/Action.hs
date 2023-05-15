{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Html.Action (toHtml) where

import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Diff qualified
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as HA
import Types qualified

toHtml :: Types.DeviceMap -> Diff.Action -> H.Html
toHtml devices (Diff.Added mdl brnch) =
  let name = getInfo devices mdl
   in do
        modelToHtml mdl
        H.td $ do
          "Added"
          name
          "branch"
          brnchsAdded brnch
toHtml devices (Diff.Removed mdl brnch) =
  let name = getInfo devices mdl
   in do
        modelToHtml mdl
        H.td $ do
          "Removed"
          name
          "branch"
          brnchsRemoved brnch
toHtml devices (Diff.Switched mdl brnch1 brnch2) =
  let name = getInfo devices mdl
   in do
        modelToHtml mdl
        H.td $ do
          name
          "switched from"
          brnchsSwitched brnch1
          "to"
          brnchsSwitched brnch2

-- toH :: [T.Text] -> H.Html
-- toH = H.td . H.toHtml . T.intercalate " "

getInfo :: Types.DeviceMap -> Types.Model -> H.Html
getInfo (Types.DeviceMap devices) mdl =
  case HM.lookup mdl devices of
    Just (Types.OEM o, Types.Name n) -> H.toHtml $ T.intercalate " " [o, n]
    _ -> error "device not found, could not be"

-- brnchsToText :: [Types.Branch] -> T.Text
-- brnchsToText = T.intercalate ", " . map (\(Types.Branch b) -> b)

brnchsAdded :: [Types.Branch] -> H.Html
brnchsAdded =
  mapM_
    (\(Types.Branch b) -> H.span (H.toHtml b) H.! HA.class_ "tag is-success")

brnchsRemoved :: [Types.Branch] -> H.Html
brnchsRemoved = brnchsAdded

brnchsSwitched :: [Types.Branch] -> H.Html
brnchsSwitched = brnchsAdded

modelToHtml :: Types.Model -> H.Html
modelToHtml (Types.Model mdl) = H.td $ H.toHtml mdl