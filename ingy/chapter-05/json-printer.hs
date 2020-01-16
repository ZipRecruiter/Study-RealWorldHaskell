
import LexJSON
import ParseJSON
import ShowJSON

import Control.Monad

main = interact $ showJSON.parseJSON.lexJSON
