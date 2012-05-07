module MiniSchemeParser (Program, Expression(..), parseMiniScheme) where

import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Error

data Expression = Atom String
                | String String
                | Bool Bool
                | Int Int
                | List [Expression] deriving (Eq,Show) 
                                     
type Program = [Expression]

parseProgram :: Parser Program
parseProgram = sepBy parseExpr spaces

parseExpr :: Parser Expression
parseExpr = parseString
        <|> parseNumber
        <|> parseBool
        <|> parseAtom
        <|> parseSExpression
        <|> parseQuoted

parseString :: Parser Expression
parseString = do char '\"'
                 str <- many1 (noneOf "\"")
                 char '\"'
                 return $ String str
                 
parseNumber :: Parser Expression
parseNumber = do numberStr <- many1 digit
                 return $ (Int . read) numberStr
                 
parseBool :: Parser Expression
parseBool = do char '#'
               bool <- char 't' <|> char 'f'
               return $ case bool of
                 't' -> Bool True
                 _   -> Bool False
                 
parseAtom :: Parser Expression
parseAtom = do firstChar <- lower
               restChars <- many alphaNum
               return $ Atom (firstChar:restChars)
               
parseSExpression :: Parser Expression
parseSExpression = do char '('
                      exprs <- sepBy parseExpr (many1 space)
                      char ')'
                      return $ List exprs
                      
parseQuoted :: Parser Expression
parseQuoted = do char '\''
                 expr <- parseExpr
                 return $ List ((Atom "quote"):expr:[])

parseMiniScheme = parse parseProgram ""