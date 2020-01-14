
import LexJSON

import Control.Monad

main = interact $ (unlines.(map show).lexJSON)
