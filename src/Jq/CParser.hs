module Jq.CParser where

import Parsing.Parsing
import Jq.Filters

-- Identity
parseIdentity :: Parser Filter
parseIdentity = do
                _ <- token . char $ '.'
                return Identity

-- Parenthesis
parseParenthesis :: Parser Filter
parseParenthesis =  do
                    _ <- token (char '(')
                    f <- parseFilter
                    _ <- token (char ')')
                    return (Parenthesis f)

-- Indexing

isGenIndexable :: Filter -> Bool
isGenIndexable Identity = True
isGenIndexable (Parenthesis _) = True
isGenIndexable (ObjectIndex _ _ _) = True 
isGenIndexable (ArrayIndex _ _ _) = True 
isGenIndexable (ArrayRange _ _ _ _) = True 
isGenIndexable _ = False 

-- Indexing -- Objects
parseObjectIdIndex :: Filter -> Parser Filter
parseObjectIdIndex f =  do
                        _ <- token (char '.') <|> (if f == Identity then return '.' else empty)
                        k <- identifier <|> parseString
                        q <- token (char '?') <|> return ' '
                        return (ObjectIndex f k (q == '?'))

parseObjectGenIndex :: Filter -> Parser Filter
parseObjectGenIndex f = do
                        _ <- token (char '.') <|> (if isGenIndexable f then return '.' else empty)
                        _ <- token (char '[') 
                        k <- parseString
                        _ <- token (char ']')
                        q <- token (char '?') <|> return ' '
                        return (ObjectIndex f k (q == '?'))

parseObjectIndex :: Filter -> Parser Filter
parseObjectIndex f = parseObjectIdIndex f <|> parseObjectGenIndex f

-- Indexing -- Arrays

parseArrayGenIndex :: Filter -> Parser Filter
parseArrayGenIndex f =  do
                        _ <- token (char '.') <|> (if isGenIndexable f then return '.' else empty)
                        _ <- token (char '[') 
                        n <- nat
                        _ <- token (char ']')
                        q <- token (char '?') <|> return ' '
                        return (ArrayIndex f n (q == '?'))

parseArrayRange :: Filter -> Parser Filter
parseArrayRange f = do
                    _ <- token (char '.') <|> (if isGenIndexable f then return '.' else empty)
                    _ <- token (char '[') 
                    n <- nat
                    _ <- token (char ':')
                    m <- nat
                    _ <- token (char ']')
                    q <- token (char '?') <|> return ' '
                    return (ArrayRange f n m (q == '?'))

parseArrayIndex :: Filter -> Parser Filter
parseArrayIndex f = parseArrayGenIndex f <|> parseArrayRange f

-- Filter
parseBinOp :: Filter -> Parser Filter
parseBinOp f =  do
                x <- parseObjectIndex f <|> parseArrayIndex f
                y <- parseBinOp x <|> return x
                return y


parseFilter :: Parser Filter
parseFilter = do 
              x <- parseParenthesis <|> parseIdentity
              y <- parseBinOp x <|> return x
              return y



parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
