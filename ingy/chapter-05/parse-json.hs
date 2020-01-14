
import LexJSON
import ParseJSON

import Control.Monad

main = interact $ (unlines.(map show).parseJSON.lexJSON)
