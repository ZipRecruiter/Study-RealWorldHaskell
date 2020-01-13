
import PrettyJSON (renderJValue)
import SimpleJSON
import Prettify (pretty)

main = mapM putStrLn $ map (\n -> show n ++ ":\n" ++ pretty n v) [1..30]
  where j = JObject [("f", JNumber 1), ("q", JBool True)]
        v = PrettyJSON.renderJValue j
