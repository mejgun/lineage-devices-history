{-# LANGUAGE ImportQualifiedPost #-}

module Lib
  ( someFunc,
  )
where

import Devices qualified

someFunc :: IO ()
someFunc = Devices.read "devices.json" >>= print
