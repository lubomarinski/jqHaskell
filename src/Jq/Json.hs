module Jq.Json where

import Numeric
import Data.Char
import Jq.NumFormat


data JSON =
    JNull |
    JNumber Double NumFormat |
    JString String |
    JBool Bool |
    JArray [JSON] |
    JObject [(String, JSON)] |
    JNothing

nestedShow :: Int -> JSON -> String
nestedShow level (JArray js) = "[" ++ foldr (\j s -> (if s == "" then "" else s ++ ",") ++ "\n" ++ concat (take level (repeat "  ")) ++ j) "" (map (nestedShow (level + 1)) (reverse js)) ++ (if length js > 0 then "\n" ++ concat (take (level - 1) (repeat "  ")) else "") ++ "]" 
nestedShow level (JObject jo) = "{" ++ foldr (\j s -> (if s == "" then "" else s ++ ",") ++ "\n" ++ concat (take level (repeat "  ")) ++ j) "" (map (\(k, v)-> show (JString k) ++ ": " ++ nestedShow (level + 1) v) (reverse jo)) ++ (if length jo > 0 then "\n" ++ concat (take (level - 1) (repeat "  ")) else "") ++ "}" 
nestedShow _ other = show other

instance Show JSON where
  show (JNull) = "null"
  show (JNumber n nf) = showNF n nf
  show (JString s) = "\"" ++ s ++ "\""
  show (JBool b) = if b then "true" else "false"
  show (JArray js) = nestedShow 1 (JArray js)
  show (JObject jo) = nestedShow 1 (JObject jo)
  show (JNothing) = ""


instance Eq JSON where
  JNull == JNull = True
  JNumber x _ == JNumber y _ = x == y
  JString x == JString y = x == y
  JBool x == JBool y = x == y
  JArray x == JArray y = x == y
  JObject x == JObject y = x == y
  JNothing == JNothing = True
  _ == _ = False


jsonNullSC :: JSON
jsonNullSC = JNull

jsonNumberSC :: Int -> JSON
jsonNumberSC n = JNumber (fromIntegral n) (NFDouble 0)

jsonStringSC :: String -> JSON
jsonStringSC s = JString s 

jsonBoolSC :: Bool -> JSON
jsonBoolSC b = JBool b

jsonArraySC :: [JSON] -> JSON
jsonArraySC xs = JArray xs

jsonObjectSC :: [(String, JSON)] -> JSON
jsonObjectSC xs = JObject xs
