{-# LANGUAGE OverloadedStrings #-}

module Html.Index (write) where

import Data.HashMap.Strict qualified as HM
import Data.List (sortOn)
import Data.Maybe (fromJust)
import Html.Header qualified
import Html.Link qualified
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as HA
import Types qualified

write :: Types.DeviceMap -> HM.HashMap Types.OEM [Types.Model] -> H.Html
write (Types.DeviceMap devices) brands =
  Html.Header.put "LineageOS Unofficial Changelog" $ do
    H.table H.! HA.class_ "table is-bordered is-striped" $
      H.tbody $ do
        H.tr $ do
          H.td Html.Link.all
          H.td Html.Link.github
        mapM_
          ( \(oem, vs) ->
              H.tr $
                do
                  H.td $ Html.Link.brand oem
                  H.td $ d vs
          )
          (sortOn (\(Types.OEM k, _) -> k) (HM.toList brands))
  where
    d = m1 . s . m2
    m1 =
      mapM_
        ( \(m@(Types.Model mn), n) -> H.p $
            do
              Html.Link.device m n
              " ("
              H.toHtml mn
              ")"
        )
    s = sortOn (\(_, Types.Name n) -> n)
    m2 = map (\m -> let (_, n) = fromJust (HM.lookup m devices) in (m, n))
