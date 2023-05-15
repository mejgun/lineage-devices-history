{-# LANGUAGE ImportQualifiedPost #-}

import DiffSpec qualified
import ParserSpec qualified

main :: IO ()
main =
  ParserSpec.main
    >> DiffSpec.main