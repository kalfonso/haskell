import Text.ParserCombinators.Parsec

csvFile = endBy line eol

line = sepBy cell (char ',')

cell = many (noneOf ",\n")

eol = do char '\n' 
         char '\r' <|> return '\n' -- Solving the lookahead problem we need to expand the options
         
parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(csvParser)" input