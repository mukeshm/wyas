module Main where

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

parseBool :: Parser LispVal
parseBool = do
              char '#'
              (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             deriving (Show)

escapedChars :: Parser Char
escapedChars = do 
                 char '\\'
                 x <- oneOf "\\\"nrt"
                 return $ case x of 
                  '\\' -> x
                  '"'  -> x
                  'n'  -> '\n'
                  'r'  -> '\r'
                  't'  -> '\t'

parseString :: Parser LispVal
parseString = do 
                 char '"'
                 x <- many $ escapedChars <|> noneOf "\"\\"
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ Atom atom
               -- return $ case atom of 
               --            "#t" -> Bool True
               --            "#f" -> Bool False
               --            otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

parseDecimal1 :: Parser LispVal
parseDecimal1 = many1 digit >>= (return . Number . read)

parseDecimal2 :: Parser LispVal
parseDecimal2 = do
                  try $ string "#d"
                  x <- many1 digit
                  (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do 
              try $ string "#x"
              x <- many1 hexDigit
              return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do
              try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do
              try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dig x)

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1)
                         in bin2dig' old xs

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseFloat
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter

parseCharacter :: Parser LispVal
parseCharacter = do
                    try $ string "#\\"
                    value <- try (string "newline" <|> string "space") 
                            <|> do {x <- anyChar; notFollowedBy alphaNum ; return [x]}
                    return $ Character $ case value of
                            "space" -> ' '
                            "newline" -> '\n'
                            otherwise -> (value !! 0 )

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (fst . head $ readFloat (x++"."++y))