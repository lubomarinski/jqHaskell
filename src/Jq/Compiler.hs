module Jq.Compiler where

import           Jq.Filters
import           Jq.Json
import           Data.List (find)


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile (Identity) inp = return [inp]
compile (Parenthesis f) inp = compile f inp
compile (ObjectIndex f k q) inp = case (compile f inp) of 
                                    Right [(JObject pairs)] -> case (find (\(s, j) -> s == k) pairs) of
                                        Just (_, j) -> return [j]
                                        _ -> return [JNull]
                                    _ -> if q then return [] else Left ("Cannot access property '" ++ k ++ "'. Not an object") 
compile (ArrayIndex f n) inp = case (compile f inp) of 
                                    Right [(JArray arr)] -> return [(arr !! n)]
                                    _ -> Left ("Cannot access item '" ++ show n ++ "'. Not an array")  
compile (ArrayRange f n m) inp = case (compile f inp) of 
                                    Right [(JArray arr)] -> return [(JArray (take (m - n) $ drop n $ arr))]
                                    _ -> Left ("Cannot access item '" ++ show n ++ "'. Not an array")

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
