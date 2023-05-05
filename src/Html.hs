{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Html (saveDiffs) where

import Control.Monad (forM_)
import Data.Text.Lazy.IO qualified as T
import Diff qualified
import Text.Blaze.Html.Renderer.Text qualified as RT
import Text.Blaze.Html5 as H

saveDiffs :: [Diff.Diffs] -> IO ()
saveDiffs xs =
  let h = H.html $ do
        H.head (H.title "All")
        H.body $ do
          H.h1 "hey"
          H.table $ forM_ xs diffToHtml
   in T.writeFile "all.html" $ RT.renderHtml h

diffToHtml :: Diff.Diffs -> H.Html
diffToHtml ((_, ct), xs) = H.tr $ do
  H.td $ H.p $ H.toHtml (show ct)
  H.td $ H.ul $ forM_ xs (H.li . H.p . H.toHtml . show)
