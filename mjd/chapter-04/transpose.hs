

import Interact(mainWithLines)
import Data.Maybe

-- f is a function that may be undefined on empty lists
-- map it over its argument lists, but ignore any empty ones
mapSafe f ls = map f $ filter (not . null) ls

-- f is a function that may be undefined on empty lists
-- map it over its argument lists, but if any are empty use
-- the value of x instead
mapSafe' f x = map (\ls -> if null ls then x else f ls)


-- f is a function that may be undefined on empty lists
-- Try to apply it to a list, and Maybe return the result
apSafe f [] = Nothing
apSafe f ls = Just $ f ls

-- This works fine when the input lists are all the same length,
-- and produces a reasonable output when they are not
transpose1 [] = []
transpose1 lss = (mapSafe' head ' ' lss) : transpose1 (mapSafe tail lss)

-- I think this handles the weird cases adequately
-- but so much code!
-- f is the fill symbol
transpose2 f lss = let heads = map (apSafe head) lss
                   in
                    case all isNothing heads of
                      True  -> []
                      False -> map (fromMaybe f) heads : transpose2 f (mapSafe' tail [] lss)
-- If I want to define `heads` in a where-clause,
-- how do I do that

transpose3 lss

t = ["x", "123", "45", "789", "ab"]

-- main = mainWithLines (transpose2 '*')
main = mainWithLines transpose1

