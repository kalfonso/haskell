import Text.ParserCombinators.Parsec

{- CSV Parser -}
csvFile :: GenParser Char st [[String]]
csvFile = do result <- many line
             eof
             return result
             
-- Each line contains one or more cells separated by a comma --
line :: GenParser Char st [String]
line = do result <- cells
          eol -- End of line
          return result
          
cells :: GenParser Char st [String]
cells = do first <- cellContent
           next <- remainingCells
           return (first : next)
           
remainingCells :: GenParser Char st [String]
remainingCells = (char ',' >> cells) <|> (return [])

cellContent :: GenParser Char st String
cellContent = many (noneOf ",\n")

eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
     