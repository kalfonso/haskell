import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Error

data Expression = Atom String
                | List [Expression]
                | Bool Bool deriving (Eq,Show)
               
type Name = String               

type Environment = [(Name, Expression)]
               
{-instance Show Expression where
  show (Atom val) = val
  show (List expressions) = "(" ++ expressionsVal expressions ++ ")"
  show (Bool True) = "#t"
  show (Bool False) = "#f" -}
  
expressionsVal :: [Expression] -> String
expressionsVal = unwords . map show

parseExpression :: Parser Expression
parseExpression  =  parseAtom
                <|> parseBool
                <|> parseQuoted
                <|> parseExpressions
                
parseAtom :: Parser Expression               
parseAtom = do val <- many1 lower
               return $ Atom val
               
parseBool :: Parser Expression
parseBool = do char '#'
               val <- char 't' <|> char 'f'
               return $ case val of
                 't' -> Bool True
                 _   -> Bool False

parseQuoted :: Parser Expression
parseQuoted = do char '\''
                 expr <- parseExpression
                 return $ List [(Atom "quote"), expr]
                
parseExpressions :: Parser Expression
parseExpressions = do char '('
                      expressions <- sepBy parseExpression (many1 space)
                      char ')'
                      return $ List expressions

eval :: Expression -> Environment -> Expression
eval (Atom x) env = lookupVal x env
eval x@(List [(Atom "quote"),_]) _ = x 
eval (List [(Atom "eq"),expr1,expr2]) env = Bool $ evalEqual expr1 expr2 env
eval (List [(Atom "atom"),expr]) env = Bool $ evalAtom expr env
eval (List [(Atom "car"),expr]) env = evalCar expr env
eval (List [(Atom "cdr"),expr]) env = evalCdr expr env
eval (List [(Atom "cons"),expr1,expr2]) env = evalCons expr1 expr2 env
eval (List ((Atom "cond"):exprs)) env = evalCond exprs env
eval (List ((List [(Atom "lambda"),(List params),bodyExpr]):args)) env = evalLambda params args bodyExpr env
eval (List ((Atom f):args)) env = evalFunction f args env 

evalExpression expressionStr  = do
                                expression <- parse parseExpression "" expressionStr
                                return $ eval expression []
                                
lookupVal :: String -> Environment -> Expression                                
lookupVal x [] = error $ "Variable: " ++ x ++ " has not been declared"
lookupVal x ((v,e):vs) | x == v = e
                       | otherwise = lookupVal x vs
                                
quote :: Expression -> Expression
quote expr = List [(Atom "quote"), expr]

listExpr :: [Expression] -> Expression
listExpr es = List es
                                
evalEqual :: Expression -> Expression -> Environment -> Bool                                
evalEqual expr1 expr2 env = let val1 = eval expr1 env
                                val2 = eval expr2 env
                            in val1 == val2
                                
evalAtom :: Expression -> Environment -> Bool                                
evalAtom (Atom _) _ = True
evalAtom (Bool _) _ = True
evalAtom (List []) _ = True
evalAtom (List [(Atom "quote"),(Atom _)]) _ = True
evalAtom (List [(Atom "quote"),(List [])]) _ = True
evalAtom (List [(Atom "quote"),_]) _ = False
evalAtom expr@(List expressions) env = evalAtom (eval expr env) env

evalCar :: Expression -> Environment -> Expression
evalCar (List [(Atom "quote"),(List [])]) _ = error "'car' requires a non empty list"
evalCar (List [(Atom "quote"),(List xs)]) _ = quote $ head xs
evalCar (List [expr]) env = evalCar (eval expr env) env
evalCar x@(Atom _) env = evalCar (eval x env) env
evalCar _ _ = error "'car' requires a list of atoms"

evalCdr :: Expression -> Environment -> Expression
evalCdr (List [(Atom "quote"),(List [])]) _ = error "'cdr' requires a non empty list"
evalCdr (List [(Atom "quote"),(List xs)]) _ = (quote . listExpr) $ tail xs
evalCdr (List [expr]) env = evalCdr (eval expr env) env
evalCdr x@(Atom _) env = evalCdr (eval x env) env
evalCdr _ _ = error "'cdr' requires a non empty list of atoms"

evalCons :: Expression -> Expression -> Environment -> Expression
evalCons (List [(Atom "quote"), x@(Atom _)]) (List [(Atom "quote"), (List xs)]) env = (quote . listExpr) $ (x:xs) 
evalCons expr1 expr2 env = evalCons (eval expr1 env) (eval expr2 env) env

evalCond :: [Expression] -> Environment -> Expression
evalCond [] _ = Bool False
evalCond ((List [predicate,expression]):condExprs) env = case (eval predicate env) of
                                                           Bool True  -> eval expression env
                                                           Bool False -> evalCond condExprs env
                                                           _          -> error $ "Predicate: " ++ (show predicate) ++ " must evaluate to True or False"
evalCond expressions _ = error $ "Expression: " ++ (show expressions) ++ " must have the form (p1 e1) ... (pn en)"

evalLambda :: [Expression] -> [Expression] -> Expression -> Environment -> Expression
evalLambda params args expr env = let argsValues = map (\arg -> eval arg env) args 
                                      paramsEnv = zip (paramNames params) argsValues
                                  in eval expr (env ++ paramsEnv)

paramNames :: [Expression] -> [String]                                     
paramNames [] = []
paramNames ((Atom x):params) = x : (paramNames params)
paramNames (param:_) = error $ "Parameter: " ++ (show param) ++ " is not a valid parameter name."

evalFunction :: String -> [Expression] -> Environment -> Expression
evalFunction name args env = let f = lookupVal name env
                             in eval (makeLambda f args) env
                                
makeLambda :: Expression -> [Expression] -> Expression                                
makeLambda (List [(Atom "quote"),funcDef@(List [(Atom "lambda"),(List _),_])]) args = (List (funcDef:args)) 