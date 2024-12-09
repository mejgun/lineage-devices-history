{-# LANGUAGE OverloadedStrings #-}

module Html.Header (put) where

import Data.Text qualified as T
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as HA

css :: H.AttributeValue
css = "https://cdn.jsdelivr.net/npm/bulma@1.0.2/css/bulma.min.css"

put :: T.Text -> H.Html -> H.Html
put hdr x = H.html $ do
  H.head $ do
    H.meta H.! HA.charset "utf-8"
    H.meta H.! HA.name "viewport" H.! HA.content "width=device-width, initial-scale=1"
    H.link H.! HA.rel "stylesheet" H.! HA.href css
    H.title $ H.toHtml hdr
  H.body x