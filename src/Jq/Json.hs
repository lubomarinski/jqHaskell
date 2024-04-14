module Jq.Json where

import Numeric
import Data.Char

data JNum = Int | Double

data JSON =
    JNull |
    JNumber Int |
    JString String |
    JBool Bool |
    JArray [JSON] |
    JObject [(String, JSON)]

nestedShow :: Int -> JSON -> String
nestedShow level (JArray js) = "[" ++ foldr (\j s -> (if s == "" then "" else s ++ ",") ++ "\n" ++ concat (take level (repeat "  ")) ++ j) "" (map (nestedShow (level + 1)) (reverse js)) ++ (if length js > 0 then "\n" ++ concat (take (level - 1) (repeat "  ")) else "") ++ "]" 
nestedShow level (JObject jo) = "{" ++ foldr (\j s -> (if s == "" then "" else s ++ ",") ++ "\n" ++ concat (take level (repeat "  ")) ++ j) "" (map (\(k, v)-> show (JString k) ++ ": " ++ nestedShow (level + 1) v) (reverse jo)) ++ (if length jo > 0 then "\n" ++ concat (take (level - 1) (repeat "  ")) else "") ++ "}" 
nestedShow _ other = show other

intToHexStr :: Int -> Int -> String
intToHexStr n l = (replicate (l - (length hexStr)) '0') ++ hexStr
  where hexStr = (showIntAtBase 16 intToDigit n "")  

escapeControlChar :: Char -> String
escapeControlChar c 
  | d == 8  = "\\b"
  | d == 9  = "\\t"
  | d == 10 = "\\n"
  | d == 12 = "\\f"
  | d == 13 = "\\r"
  | d == 34 = "\\\""
  | d == 92 = "\\\\"
  | d < 32 = "\\u" ++ (intToHexStr d 4)
  | otherwise = [c]
  where d = ord c

instance Show JSON where
  show (JNull) = "null"
  show (JNumber n) = show n
  show (JString s) = "\"" ++ (s >>= escapeControlChar) ++ "\""
  show (JBool b) = if b then "true" else "false"
  show (JArray js) = nestedShow 1 (JArray js)
  show (JObject jo) = nestedShow 1 (JObject jo)


instance Eq JSON where
  JNull == JNull = True
  JNumber x == JNumber y = x == y
  JString x == JString y = x == y
  JBool x == JBool y = x == y
  JArray x == JArray y = x == y
  JObject x == JObject y = x == y
  _ == _ = False


jsonNullSC :: JSON
jsonNullSC = JNull

jsonNumberSC :: Int -> JSON
jsonNumberSC n = JNumber n

jsonStringSC :: String -> JSON
jsonStringSC s = JString s 

jsonBoolSC :: Bool -> JSON
jsonBoolSC b = JBool b

jsonArraySC :: [JSON] -> JSON
jsonArraySC xs = JArray xs

jsonObjectSC :: [(String, JSON)] -> JSON
jsonObjectSC xs = JObject xs
