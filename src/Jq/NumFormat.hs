module Jq.NumFormat where

import Data.List
import Numeric

data NumFormat = NFDouble Int | NFE
    deriving (Eq)


reformatE :: String -> String
reformatE ('.':'0':'e':xs) = "E+" ++ xs
reformatE ('e':xs) = "E+" ++ xs
reformatE (x:xs) = x : (reformatE xs)
reformatE [] = []


showNF :: Double -> NumFormat -> String
showNF n (NFDouble 0) = if isSuffixOf ".0" nstr then reverse (drop 2 (reverse nstr)) else nstr
                            where nstr = showFFloat Nothing n ""
showNF n (NFDouble d) = showFFloat (Just d) n ""
showNF n NFE = reformatE (showEFloat Nothing n "")
