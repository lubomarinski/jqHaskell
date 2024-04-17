module Jq.Compiler where

import           Jq.Filters
import           Jq.Json
import           Data.List (find)


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile (FIdentity) inp =            
    return [inp]
compile (FParenthesis f) inp =       
    compile f inp
compile (FGenIndex f i q) inp =      
    let compiledIndex = compile i inp
    in  compile f inp >>= \xs -> sequence $ xs >>= \x -> 
            sequence $ compiledIndex >>= \ys -> sequence $ ys >>= \y -> case y of
                JNothing -> case x of
                    (JObject pairs) -> map (\(_, j) -> Right j) pairs
                    (JArray arr) -> map (\elem -> Right elem) arr
                    _ -> if q then [] else [Left ("Cannot iterate over value. Not an object or an array")] 
                (JString k) -> case x of
                    (JObject pairs) -> case (find (\(s, _) -> s == k) pairs) of
                        Just (_, j) -> [Right j]
                        _ -> [Right JNull]
                    _ -> if q then [] else [Left ("Cannot access property '" ++ k ++ "'. Not an object")] 
                (JNumber n _) -> case x of
                    (JArray arr) -> [Right (arr !! (round n))]
                    _ -> if q then [] else [Left ("Cannot access item " ++ show n ++ ". Not an array")]
compile (FArrayRange f fn fm q) inp =  
    case (compile fn inp, compile fm inp) of
        (Right [(JNumber nd _)], Right [JNumber md _]) -> 
            let n = round nd
                m = round md
            in  compile f inp >>= \xs -> sequence $ xs >>= \x -> case x of 
                    (JArray arr) -> [Right (JArray (take (m - n) $ drop n $ arr))]
                    _ -> if q then [] else [Left ("Cannot access items " ++ show n ++ " and " ++ show m ++ ". Not an array")]
        _ -> Left "Cannot compile array range indices"
compile (FLiteral j) inp = Right [j]
compile (FComma b a) inp = sequence $ (sequence $ compile b inp) ++ (sequence $ compile a inp)
compile (FPipe b a) inp = compile b inp >>= \xs -> sequence $ xs >>= \x -> sequence $ compile a x
compile (FRecDesc f) inp = sequence $ sequence (compile f inp) ++ (sequence $ compile (FGenIndex f (FLiteral JNothing) True) inp >>= \xs -> sequence $ xs >>= \x -> sequence $ compile (FRecDesc FIdentity) x)
compile (FArray f) inp = compile f inp >>= \xs -> Right [JArray xs]
compile (FObject fps) inp = 
    let pairVariants = sequence $ map (\(k, v) -> [compile k inp, compile v inp]) fps >>= \p -> case p of
            [Right [JString s], Right [JNothing]] -> [compile (FGenIndex FIdentity (FLiteral (JString s)) False) inp >>= \jvs -> Right (sequence [[JString s], jvs] >>= \[JString k, jv] -> [(k, jv)])]
            [Right [JString s], Right jvs] -> [Right (sequence [[JString s], jvs] >>= \[JString k, jv] -> [(k, jv)])]
            _ -> [Left "Cannot compile object constructor"]
    in pairVariants >>= \pvs -> Right (map (\ps -> JObject ps) (sequence pvs))


run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
