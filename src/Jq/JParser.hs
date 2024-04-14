module Jq.JParser where

import Parsing.Parsing
import Jq.Json

-- JNull |
-- JNumber Int |
-- JString String |
-- JBool Bool |
-- JArray [JSON] |
-- JObject [(String, JSON)]

-- Number
parseJNumber :: Parser JSON
parseJNumber =  do 
                n <- int
                return (JNumber n)

-- Null
parseJNull :: Parser JSON
parseJNull =    do 
                _ <- string "null"
                return JNull

-- String



parseJString :: Parser JSON
parseJString =  do 
                s <- parseString
                return (JString s)

-- Bool
parseJBool :: Parser JSON
parseJBool =    do 
                t <- token (string "true" <|> string "false")
                return (JBool (if t == "true" then True else False)) 

-- Array
parseJArrayContent :: Parser [JSON]
parseJArrayContent =    do
                        j <- parseJSON
                        c <- token (char ',' <|> char ']')
                        res <- (if c == ',' then pure (\js -> j : js) <*> parseJArrayContent else return [j])
                        return res

parseEmptyJArray :: Parser [JSON]
parseEmptyJArray = do
                    _ <- token(char ']')
                    return []

parseJArray :: Parser JSON
parseJArray =   do
                _ <- token (char '[')
                jarr <- (parseJArrayContent <|> parseEmptyJArray)
                return (JArray jarr)

-- Object
parseJObjectContent :: Parser [(String, JSON)]
parseJObjectContent =   do
                        k <- parseString
                        _ <- token (char ':')
                        v <- parseJSON
                        let pair = (k, v)
                        c <- token (char ',' <|> char '}')
                        res <- (if c == ',' then pure (\pairs -> pair : pairs) <*> parseJObjectContent else return [pair])
                        return res

parseEmptyJObject :: Parser [(String, JSON)]
parseEmptyJObject = do
                    _ <- token(char '}')
                    return []

parseJObject :: Parser JSON
parseJObject =  do
                _ <- token (char '{')
                pairs <- (parseJObjectContent <|> parseEmptyJObject)
                return (JObject pairs)

parseJSON :: Parser JSON
parseJSON = token $ (parseJNull <|> parseJNumber <|> parseJString <|> parseJBool <|> parseJArray <|> parseJObject)
