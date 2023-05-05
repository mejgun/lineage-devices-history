{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Html (saveDiffs) where

import Control.Monad (forM_)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Time.Format qualified as Time
import Diff qualified
import Los.BuildFile.Parser qualified as P
import Los.Devices qualified
import System.Directory qualified as D
import Text.Blaze.Html.Renderer.Pretty qualified as RP
import Text.Blaze.Html5 as H

path :: FilePath
path = "html"

saveDiffs :: Los.Devices.DeviceMap -> [Diff.Diffs] -> IO ()
saveDiffs devices xs = do
  D.setCurrentDirectory path
  forM_ (HM.keys devices) (\e -> writeHtml (T.unpack e ++ ".html") (makeDiffs e devices (filterDiffByDevice e xs)))
  writeHtml "all.html" $ makeDiffs "All" devices xs
  D.setCurrentDirectory ".."

makeDiffs :: T.Text -> Los.Devices.DeviceMap -> [Diff.Diffs] -> H.Html
makeDiffs hdr devices xs = H.html $ do
  H.head (H.title (H.toHtml hdr))
  H.body $ do
    H.h1 (H.toHtml hdr)
    H.table $ forM_ xs (diffToHtml devices)

writeHtml :: FilePath -> H.Html -> IO ()
writeHtml f h = writeFile f $ RP.renderHtml h

filterDiffByDevice :: T.Text -> [Diff.Diffs] -> [Diff.Diffs]
filterDiffByDevice d = foldr f []
  where
    f (c, difs) acc = case filter f2 difs of
      [] -> acc
      fx -> (c, fx) : acc

    f2 (Diff.Added (P.Target mdl _)) = mdl == d
    f2 (Diff.Removed (P.Target mdl _)) = mdl == d
    f2 (Diff.Switched (P.Target mdl _) _) = mdl == d

diffToHtml :: Los.Devices.DeviceMap -> Diff.Diffs -> H.Html
diffToHtml devices ((_, ct), xs) = H.tr $ do
  H.td $ H.toHtml (formatTime ct)
  H.td $ H.ul $ forM_ xs (H.li . H.p . H.toHtml . toString)
  where
    toString :: Diff.Action -> T.Text
    toString (Diff.Added (P.Target mdl brnch)) =
      T.concat ["(", mdl, ") ", "Added ", name mdl, " branch ", brnch]
    toString (Diff.Removed (P.Target mdl _)) =
      T.concat ["(", mdl, ") ", "Removed ", name mdl]
    toString (Diff.Switched (P.Target mdl brnch1) (P.Target _ brnch2)) =
      T.concat ["(", mdl, ") ", name mdl, " switched from ", brnch1, " to ", brnch2]

    name :: T.Text -> T.Text
    name m = case HM.lookup m devices of
      Just (Los.Devices.Item _ brnd nm) -> T.concat [brnd, " ", nm]
      Nothing -> error $ "unknown device" ++ show m

    formatTime = Time.formatTime Time.defaultTimeLocale "%F %R"
