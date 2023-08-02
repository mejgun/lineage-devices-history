import DiffSpec qualified
import ParserSpec qualified
import SortSpec qualified

main :: IO ()
main =
  ParserSpec.main
    >> DiffSpec.main
    >> SortSpec.main