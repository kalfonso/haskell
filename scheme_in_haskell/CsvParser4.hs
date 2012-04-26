import Text.ParserCombinators.Parsec

csvFile = endBy line eol

line :: GenParser Char st [String]
line = sepBy cell (char ',')

cell :: GenParser Char st String
cell = many (noneOf ",\n\r")

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
         
parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(csvParser)" input