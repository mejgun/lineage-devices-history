{-# LANGUAGE OverloadedStrings #-}

module Html.Link
  ( device,
    brand,
    Html.Link.all,
    index,
    github,
    wiki,
    hudson,
  )
where

import Data.Text qualified as T
import Data.Time qualified as Time
import Git qualified
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

all :: H.Html
all = (H.a H.! HA.href (H.textValue "/all.html")) "All devices"

index :: H.Html
index = H.a H.! HA.href (H.textValue "/index.html") $ "← Index"

github :: H.Html
github =
  H.a
    H.! HA.href
      (H.textValue "https://github.com/mejgun/lineage-devices-history")
    $ "GitHub"

wiki :: Types.Model -> H.Html
wiki (Types.Model m) =
  (H.a H.! HA.href (H.textValue (T.concat ["https://wiki.lineageos.org/devices/", m, "/"])))
    (H.toHtml m)

hudson :: Git.Commit -> H.Html
hudson (hash, time) =
  ( H.a
      H.! HA.href
        (H.textValue (T.concat ["https://github.com/LineageOS/hudson/commit/", T.pack hash, "/"]))
  )
    (H.toHtml (Time.formatTime Time.defaultTimeLocale "%F %R" time))
