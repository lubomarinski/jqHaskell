module Jq.Compiler where

import           Jq.Filters
import           Jq.Json
import           Data.List (find)


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile (Identity) inp =            return [inp]
compile (Parenthesis f) inp =       compile f inp
compile (ObjectIndex f k q) inp =   compile f inp >>= \xs -> sequence $ xs >>= \x -> case x of
                                        (JObject pairs) -> case (find (\(s, _) -> s == k) pairs) of
                                            Just (_, j) -> [Right j]
                                            _ -> [Right JNull]
                                        _ -> if q then [] else [Left ("Cannot access property '" ++ k ++ "'. Not an object")] 
compile (ArrayIndex f n q) inp =    compile f inp >>= \xs -> sequence $ xs >>= \x -> case x of
                                        (JArray arr) -> [Right (arr !! n)]
                                        _ -> if q then [] else [Left ("Cannot access item " ++ show n ++ ". Not an array")]
compile (ArrayRange f n m q) inp =  compile f inp >>= \xs -> sequence $ xs >>= \x -> case x of 
                                        (JArray arr) -> [Right (JArray (take (m - n) $ drop n $ arr))]
                                        _ -> if q then [] else [Left ("Cannot access items " ++ show n ++ " and " ++ show m ++ ". Not an array")]

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
