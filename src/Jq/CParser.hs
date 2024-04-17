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

parseGenIndex :: Filter -> Parser (Bool -> Filter)
parseGenIndex f = do
                  _ <- token (char '.') <|> (if isGenIndexable f then return '.' else empty)
                  _ <- token (char '[') 
                  i <- parseFilter <|> return (FLiteral JNothing)
                  _ <- token (char ']')
                  return (FGenIndex f i)

-- Indexing -- Objects
parseObjectIdIndex :: Filter -> Parser (Bool -> Filter)
parseObjectIdIndex f =  do
                        _ <- token (char '.') <|> (if f == FIdentity then return '.' else empty)
                        i <- parseStringIdentifier
                        return (FGenIndex f i)

-- Indexing -- Arrays
parseFArrayRange :: Filter -> Parser (Bool -> Filter)
parseFArrayRange f = do
                    _ <- token (char '.') <|> (if isGenIndexable f then return '.' else empty)
                    _ <- token (char '[') 
                    n <- parseFilter <|> return (FLiteral JNothing)
                    _ <- token (char ':')
                    m <- parseFilter <|> if n == FLiteral JNothing then empty else return (FLiteral JNothing)
                    _ <- token (char ']')
                    return (FArrayRange f n m)

parseIndex :: Filter -> Parser Filter
parseIndex f =  do 
                i <- parseFArrayRange f <|> parseGenIndex f <|> parseObjectIdIndex f
                q <- token (char '?') <|> return ' '
                return (i (q == '?'))

-- Recursive Descent
parseRecDesc :: Filter -> Parser Filter
parseRecDesc f =  do
                  _ <- symbol ".."
                  return (FRecDesc f)

-- Comma
parseComma :: Filter -> Parser Filter
parseComma b =  do
                _ <- token (char ',')
                a <- parseWithComma
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
                f <- parseFilter <|> return (FLiteral JNothing)
                _ <- token (char ']')
                return (FArray f)

-- Object
parseFObjectPair :: Parser [(Filter, Filter)]
parseFObjectPair =  do
                    let parseFullPair = (parseStatement <|> parseStringIdentifier) >>= \k -> token (char ':') >>= \_ -> parseStatement >>= \v -> return (k, v)
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

-- Try Catch
parseFTryCatch :: Parser Filter
parseFTryCatch =  do
                  _ <- symbol "try"
                  t <- parseFilter
                  _ <- symbol "catch"
                  c <- parseFilter
                  return (FTryCatch t c)

-- Filter
parseRepeating :: Filter -> [Filter -> Parser Filter] -> (Parser Filter)
parseRepeating f rs = do
                      x <- foldr (\r a -> r f <|> a) empty rs 
                      y <- parseRepeating x rs <|> return x
                      return y

parseSingular :: (Parser Filter) -> [Filter -> Parser Filter] -> (Parser Filter)
parseSingular sing rs = do
                        x <- sing
                        y <- parseRepeating x rs <|> return x
                        return y


parseStatement :: Parser Filter
parseStatement = parseSingular (parseFParenthesis <|> parseRecDesc FIdentity <|> parseFIdentity <|> parseLiterals <|> parseFArray <|> parseFObject <|> parseFTryCatch) [parseIndex, parseRecDesc]

parseWithComma :: Parser Filter
parseWithComma =  parseSingular parseStatement [parseComma]

parseWithPipe :: Parser Filter
parseWithPipe = parseSingular parseWithComma [parsePipe]

parseFilter :: Parser Filter
parseFilter = parseWithPipe



parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
