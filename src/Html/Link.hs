{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Html.Link (device, brand) where

import Data.Text qualified as T
import Text.Blaze.Html qualified as H
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as HA
import Types qualified

a :: T.Text -> T.Text -> T.Text -> H.Html
a path file text =
  H.a
    H.! HA.href
      (H.textValue (T.concat ["/", path, "/", file, ".html"]))
    $ H.toHtml text

device :: Types.Model -> Types.Name -> H.Html
device (Types.Model m) (Types.Name n) = a "device" m n

brand :: Types.OEM -> H.Html
brand (Types.OEM o) = a "brand" o o