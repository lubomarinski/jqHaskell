{-# OPTIONS_GHC -w #-}
-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

module Parsing.Parsing (module Parsing.Parsing, module Control.Applicative) where

import Control.Applicative
import Data.Char
import Jq.NumFormat

-- Basic definitions

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

-- Sequencing parsers

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

-- Making choices

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\inp -> [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           [(v,out)] -> [(v,out)])

-- Derived primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many (alphanum <|> char '_')
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

-- Handling spacing

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)


-- String
sitem :: Parser String
sitem = do
        x <- item
        return [x]


parseStringContent :: Parser String
parseStringContent =   do 
                        x <- ((string "\\\"") <|> sitem)
                        xs <- (if x == "\"" then return "" else pure (\s -> x ++ s) <*> parseStringContent)
                        return xs

parseString :: Parser String
parseString =  do 
                _ <- char '\"'
                s <- parseStringContent
                return s

-- Number
parseDouble :: Parser (Double, NumFormat)
parseDouble =  do
               wp <- some digit <|> return ""
               d <- char '.' <|> return ' '
               dp <- some digit <|> return ""
               case (wp, d, dp) of
                  ("", _, "") -> empty
                  ("",'.', dp) -> return (read ("0." ++ dp), NFDouble (length dp))
                  (wp, _, "") -> return (read wp, NFDouble 0)
                  (wp,'.', dp) -> return (read (wp ++ "." ++ dp), NFDouble (length dp))
                  _ -> empty

parseE :: Parser (Double, NumFormat)
parseE = do
         (n, nf) <- parseDouble
         _ <- char 'E' <|> char 'e'
         s <- char '+' <|> char '-' <|> return ' '
         e <- nat
         let nnf =   (case nf of 
                        (NFDouble 0) -> NFE
                        _ -> NFDouble 0) 
         return (read (show n ++ "E" ++ (if s == '-' then "-" else "") ++ show e), if s == '-' then NFDouble 0 else nnf)

parsePosNumber :: Parser (Double, NumFormat)
parsePosNumber = parseE <|> parseDouble

parseNumber :: Parser (Double, NumFormat)
parseNumber =  do
               s <- char '-' <|> return ' '
               (n, nf) <- parsePosNumber
               return (if s == '-' then -n else n, nf)