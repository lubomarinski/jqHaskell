module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.Json
import Jq.JParser

-- General
parseStringIdentifier :: Parser Filter
parseStringIdentifier = do
                        i <- identifier <|> parseString
                        return (FLiteral (JString i))

-- Literals
parseLiterals :: Parser Filter
parseLiterals = do
                l <- parseJNull <|> parseJNumber <|> parseJString <|> parseJBool
                return (FLiteral l)


-- FIdentity
parseFIdentity :: Parser Filter
parseFIdentity = do
                _ <- token . char $ '.'
                return FIdentity

-- FParenthesis
parseFParenthesis :: Parser Filter
parseFParenthesis =  do
                    _ <- token (char '(')
                    f <- parseFilter
                    _ <- token (char ')')
                    return (FParenthesis f)

-- Indexing
isGenIndexable :: Filter -> Bool
isGenIndexable (FComma _ _) = False
isGenIndexable (FPipe _ _) = False
isGenIndexable _ = True 


parseGenIndex :: Filter -> Parser Filter
parseGenIndex f = do
                  _ <- token (char '.') <|> (if isGenIndexable f then return '.' else empty)
                  _ <- token (char '[') 
                  i <- parseFilter <|> return (FLiteral JNothing)
                  _ <- token (char ']')
                  q <- token (char '?') <|> return ' '
                  return (FGenIndex f i (q == '?'))

parseRecDesc :: Filter -> Parser Filter
parseRecDesc f =  do
                  _ <- symbol ".."
                  return (FRecDesc f)

-- Indexing -- Objects
parseObjectIdIndex :: Filter -> Parser Filter
parseObjectIdIndex f =  do
                        _ <- token (char '.') <|> (if f == FIdentity then return '.' else empty)
                        i <- parseStringIdentifier
                        q <- token (char '?') <|> return ' '
                        return (FGenIndex f i (q == '?'))

-- Indexing -- Arrays
parseFArrayRange :: Filter -> Parser Filter
parseFArrayRange f = do
                    _ <- token (char '.') <|> (if isGenIndexable f then return '.' else empty)
                    _ <- token (char '[') 
                    n <- parseFilter
                    _ <- token (char ':')
                    m <- parseFilter
                    _ <- token (char ']')
                    q <- token (char '?') <|> return ' '
                    return (FArrayRange f n m (q == '?'))

parseIndex :: Filter -> Parser Filter
parseIndex f = parseFArrayRange f <|> parseGenIndex f <|> parseObjectIdIndex f <|> parseRecDesc f

-- Comma
parseComma :: Filter -> Parser Filter
parseComma b =  do
                _ <- token (char ',')
                a <- parseFilter
                return (FComma b a)

-- Pipe
parsePipe :: Filter -> Parser Filter
parsePipe b = do
              _ <- token (char '|')
              a <- parseFilter
              return (FPipe b a)

-- Array
parseFArray :: Parser Filter
parseFArray = do
                _ <- token (char '[')
                f <- parseFilter
                _ <- token (char ']')
                return (FArray f)

-- Object
parseFObjectPair :: Parser [(Filter, Filter)]
parseFObjectPair =  do
                    let parseFullPair = (parseSingular <|> parseStringIdentifier) >>= \k -> token (char ':') >>= \_ -> parseSingular >>= \v -> return (k, v)
                    let parseIdPair = parseStringIdentifier >>= \k -> return (k, FLiteral JNothing)
                    p <- parseFullPair <|> parseIdPair
                    return [p]                     

parseFObject :: Parser Filter
parseFObject =  do
                _ <- token (char '{')
                ps <- many (parseFObjectPair >>= \p -> token (char ',') >>= \_ -> return p)
                p <- parseFObjectPair <|> if length ps > 0 then empty else return []
                _ <- token (char '}')
                return (FObject (p ++ (concat ps)))

-- Filter
parseSingExt :: Filter -> Parser Filter
parseSingExt f =  do
                  x <- parseIndex f -- parseSingExt List
                  y <- parseSingExt x <|> return x
                  return y

parseSingular :: Parser Filter
parseSingular = do 
                x <- parseFParenthesis <|> parseRecDesc FIdentity <|> parseFIdentity <|> parseLiterals <|> parseFArray <|> parseFObject -- statement list
                y <- parseSingExt x <|> return x
                return y

parseBinOp :: Filter -> Parser Filter
parseBinOp f =  do
                x <- parseComma f <|> parsePipe f -- BinOp List
                y <- parseBinOp x <|> return x
                return y

parseFilter :: Parser Filter
parseFilter = do 
              x <- parseSingular
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
