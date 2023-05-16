{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Html (saveDiffs) where

import Control.Monad (forM_)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Time.Format qualified as Time
import Diff qualified
import Git qualified
import Html.Action qualified
import System.Directory qualified as D
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as HA
import Types qualified

path :: FilePath
path = "html"

css :: H.AttributeValue
css = "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"

type Diffs = (Git.Commit, [Diff.Action])

saveDiffs :: Types.DeviceMap -> [Diffs] -> IO ()
saveDiffs dm@(Types.DeviceMap devices) xs = do
  D.setCurrentDirectory path
  let deviceDiffs =
        filter (not . null . snd) $
          map (\m -> (m, filterDiffByDevice xs [m])) (HM.keys devices)
  forM_
    deviceDiffs
    ( \(Types.Model m, diffs) ->
        writeHtml
          (T.unpack (T.concat ["device/", m, ".html"]))
          (makeDiffs m dm diffs)
    )
  let l =
        HM.foldrWithKey
          ( \m (o, _) acc ->
              let old = HM.lookupDefault [] o acc
               in HM.insert o (m : old) acc
          )
          HM.empty
          devices
  let brandDiffs =
        filter (not . null . snd) $
          map (\(brnd, mdls) -> (brnd, filterDiffByDevice xs mdls)) (HM.toList l)
  forM_
    brandDiffs
    ( \(Types.OEM brnd, diffs) ->
        writeHtml
          (T.unpack (T.concat ["brand/", brnd, ".html"]))
          (makeDiffs brnd dm diffs)
    )
  writeHtml "all.html" $ makeDiffs "All" dm xs
  D.setCurrentDirectory ".."

makeDiffs :: T.Text -> Types.DeviceMap -> [Diffs] -> H.Html
makeDiffs hdr _ [] = error $ "empty diff for " ++ T.unpack hdr
makeDiffs hdr devices xs = H.html $ do
  H.head $ do
    H.meta H.! HA.charset "utf-8"
    H.meta H.! HA.name "viewport" H.! HA.content "width=device-width, initial-scale=1"
    H.link H.! HA.rel "stylesheet" H.! HA.href css
    H.title $ H.toHtml hdr
  H.body $ do
    H.h1 (H.toHtml hdr)
    H.table H.! HA.class_ "table is-bordered is-striped" $
      H.tbody $
        forM_ xs (diffToHtml devices)

writeHtml :: FilePath -> H.Html -> IO ()
writeHtml f h = writeFile f $ renderHtml h

filterDiffByDevice :: [Diffs] -> [Types.Model] -> [Diffs]
filterDiffByDevice xs d = foldr f [] xs
  where
    f (c, difs) acc = case filter f2 difs of
      [] -> acc
      fx -> (c, fx) : acc

    f2 (Diff.Added mdl _) = mdl `elem` d
    f2 (Diff.Removed mdl _) = mdl `elem` d
    f2 (Diff.Switched mdl _ _) = mdl `elem` d

diffToHtml :: Types.DeviceMap -> Diffs -> H.Html
diffToHtml devices ((_, ct), xs) =
  case xs of
    [] -> error "actions list empty"
    [x] -> H.tr $ do cmtSingle; diff x
    (h : t) -> do
      H.tr $ do
        cmtRow
        diff h
      mapM_ (H.tr . diff) t
  where
    cmtRow = H.td H.! HA.rowspan (H.stringValue (show (length xs))) $ H.toHtml (formatTime ct)

    cmtSingle = H.td $ H.toHtml (formatTime ct)

    diff = Html.Action.toHtml devices

    formatTime = Time.formatTime Time.defaultTimeLocale "%F %R"
