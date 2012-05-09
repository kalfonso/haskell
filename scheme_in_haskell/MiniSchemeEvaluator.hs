module MiniSchemeEvaluator (evalMiniScheme) where

import MiniSchemeParser

type Name = String

type Environment = [(Name,Expression)]

data EvalState = EvalState Environment Expression deriving Show

evalExpr :: Expression -> EvalState -> EvalState
evalExpr (List [Atom "quote", expr]) evalState = evalQuote expr evalState
evalExpr (List [Atom "atom", expr]) evalState = evalIsAtom expr evalState
evalExpr (List [Atom "eq", expr1, expr2]) evalState = evalEq expr1 expr2 evalState
evalExpr (List [Atom "cons", expr1, expr2]) evalState = evalCons expr1 expr2 evalState
evalExpr (List [Atom "car", expr1]) evalState = evalCar expr1 evalState
evalExpr (List [Atom "cdr", expr1]) evalState = evalCdr expr1 evalState
evalExpr expr _ = error $ show expr 

evalQuote :: Expression -> EvalState -> EvalState
evalQuote expr (EvalState env _) = EvalState env expr

evalIsAtom :: Expression -> EvalState -> EvalState
evalIsAtom (List [Atom "quote",Atom _]) (EvalState env _) = EvalState env $ Bool True
evalIsAtom (List [Atom "quote",Bool _]) (EvalState env _) = EvalState env $ Bool True
evalIsAtom (List [Atom "quote",Int _]) (EvalState env _) = EvalState env $ Bool True
evalIsAtom (List [Atom "quote",_]) (EvalState env _) = EvalState env $ Bool False
evalIsAtom expr evalState = evalExpr expr evalState

evalEq :: Expression -> Expression -> EvalState -> EvalState
evalEq (List [Atom "quote", Atom x]) (List [Atom "quote", Atom y]) (EvalState env _) = let eq = x == y
                                                                                       in EvalState env $ Bool eq
evalEq (List [Atom "quote", List []]) (List [Atom "quote", List[]]) (EvalState env _) = EvalState env $ Bool True                                                                                          
evalEq (List [Atom "quote", _]) (List [Atom "quote", _]) (EvalState env _) = EvalState env $ Bool False
evalEq expr1 expr2 evalState = let (EvalState _ expr1') = evalExpr expr1 evalState
                                   (EvalState _ expr2') = evalExpr expr2 evalState
                               in evalEq expr1' expr2' evalState
                                   
evalCons :: Expression -> Expression -> EvalState -> EvalState                                  
evalCons (List [Atom "quote", Atom x]) (List [Atom "quote", List xs]) (EvalState env _) = let car = List [Atom "quote", List ((Atom x):xs)]
                                                                                         in EvalState env car
evalCons (List [Atom "quote", List _]) _ _ = error $ "'car' requires first argument to be an atom"
evalCons _ (List [Atom "quote", Atom _]) _ = error $ "'car' requires second argument to be a list"
evalCons expr1 expr2 evalState = let (EvalState _ expr1') = evalExpr expr1 evalState
                                     (EvalState _ expr2') = evalExpr expr2 evalState
                                 in evalCons expr1' expr2' evalState
                                    
evalCar :: Expression -> EvalState -> EvalState                                    
evalCar (List [Atom "quote", List xs]) (EvalState env _) = let car = quoteList $ [head xs]
                                                           in EvalState env car
evalCar (List [Atom "quote", _]) _ = error "'car' requires a list argument"
evalCar expr1 evalState = let evalState' = evalExpr expr1 evalState
                          in evalCar (getExpr evalState') evalState'
                             
evalCdr :: Expression -> EvalState -> EvalState                             
evalCdr (List [Atom "quote", List xs]) (EvalState env _) = let cdr = quoteList $ tail xs
                                                           in EvalState env cdr 
evalCdr (List [Atom "quote", _]) _ = error "'cdr' requires a list argument"
evalCdr expr1 evalState = let evalState' = evalExpr expr1 evalState
                          in evalCdr (getExpr evalState') evalState'

evalMiniScheme program = do (expr:exprs) <- parseMiniScheme program
                            return $ evalExpr expr (EvalState [] void)
                            
void :: Expression                           
void = List []

getExpr :: EvalState -> Expression
getExpr (EvalState _ expr) = expr

getEnv :: EvalState -> Environment
getEnv (EvalState env _) = env

quoteList :: [Expression] -> Expression
quoteList exprs = List ((Atom "quote"):exprs)