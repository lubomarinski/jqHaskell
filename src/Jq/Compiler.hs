module Jq.Compiler where

import           Jq.Filters
import           Jq.Json
import           Data.List (find, sortBy)


type JProgram a = JSON -> Either String a

compileRange :: Filter -> Filter -> Int -> JSON -> Either String (Int, Int)
compileRange fn fm l inp =  (case (compile fn inp, compile fm inp) of
                                (Right [(JNumber nd _)], Right [JNumber md _]) -> Right (round nd, round md)
                                (Right [(JNumber nd _)], Right [JNothing]) -> Right (round nd, l)
                                (Right [JNothing], Right [JNumber md _]) -> Right (0, round md)
                                _ -> Left "Cannot compile array range indices") >>= \(n, m) -> 
                                    let n1 = (if n < 0 then n + l else n)
                                        m1 = (if m < 0 then m + l else m)
                                        nmdiff = if (m1 - n1) > 0 then (m1 - n1) else 0
                                    in return (if n1 > 0 then n1 else 0, nmdiff)

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
                    (JNull) -> [Right JNull]
                    _ -> if q then [] else [Left ("Cannot access property '" ++ k ++ "'. Not an object")] 
                (JNumber n _) -> case x of
                    (JArray arr) -> if n1 < 0 || n1 >= length arr then [Right JNull] else [Right (arr !! n1)]
                        where n1 = (if round n < 0 then round n + (length arr) else round n)
                    (JNull) -> [Right JNull]
                    _ -> if q then [] else [Left ("Cannot access item " ++ show n ++ ". Not an array")]
compile (FArrayRange f fn fm q) inp =  
    compile f inp >>= \xs -> sequence $ xs >>= \x -> case x of 
        (JArray arr) -> sequence $ (compileRange fn fm (length arr) inp) >>= \(n, nmdiff) -> 
                            Right [(JArray (take nmdiff $ drop n $ arr))]
        (JString s) -> sequence $ (compileRange fn fm (length s) inp) >>= \(n, nmdiff) -> 
                            Right [(JString (take nmdiff $ drop n $ s))]
        (JNull) -> [Right JNull]
        _ -> if q then [] else [Left ("Cannot access range. Not an array")]
compile (FLiteral j) inp = Right [j]
compile (FComma b a) inp = sequence $ (sequence $ compile b inp) ++ (sequence $ compile a inp)
compile (FPipe b a) inp = compile b inp >>= \xs -> sequence $ xs >>= \x -> sequence $ compile a x
compile (FRecDesc f) inp = sequence $ sequence (compile f inp) ++ (sequence $ compile (FGenIndex f (FLiteral JNothing) True) inp >>= \xs -> sequence $ xs >>= \x -> sequence $ compile (FRecDesc FIdentity) x)
compile (FArray f) inp = compile f inp >>= \xs -> Right [JArray (if xs == [JNothing] then [] else xs)]
compile (FObject fps) inp = 
    let pairVariants = sequence $ map (\(k, v) -> [compile k inp, compile v inp]) fps >>= \p -> case p of
            [Right [JString s], Right [JNothing]] -> [compile (FGenIndex FIdentity (FLiteral (JString s)) False) inp >>= \jvs -> Right (sequence [[JString s], jvs] >>= \[JString k, jv] -> [(k, jv)])]
            [Right [JString s], Right jvs] -> [Right (sequence [[JString s], jvs] >>= \[JString k, jv] -> [(k, jv)])]
            _ -> [Left "Cannot compile object constructor"]
    in pairVariants >>= \pvs -> Right (map (\ps -> JObject (sortBy (\(k1, _) (k2, _) -> compare k1 k2) ps)) (sequence pvs))


run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
