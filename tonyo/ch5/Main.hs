module Main (main) where
import Jsun
import JsunPretty

main = prJVal $ JObj [
    ("foo", JNum 1)
  , ("bar", JBool False)
  , ("xyz", JStr "hello world")
  , ("object", JObj [
      ("key1", JArr [(JBool True), (JNull), (JNum 5)])
    ])
  , ("array", JArr [(JStr "hello world2")])
  ]
