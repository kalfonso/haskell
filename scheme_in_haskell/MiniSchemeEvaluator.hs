module MiniSchemeEvaluator (evalMiniScheme) where

import MiniSchemeParser

type Name = String

type Environment = [(Name,Expression)]

data EvalState = EvalState Environment Expression deriving Show

evalExpr :: Expression -> EvalState -> EvalState
evalExpr (List [Atom "quote", expr]) evalState = evalQuote expr evalState
evalExpr (List [Atom "atom", expr]) evalState = evalIsAtom expr evalState
evalExpr (List [Atom "eq", expr1, expr2]) evalState = evalEq expr1 expr2 evalState
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
evalEq expr1 expr2 evalState = let evalState1@(EvalState _ expr1') = evalExpr expr1 evalState
                                   evalState2@(EvalState _ expr2') = evalExpr expr2 evalState1
                               in evalEq expr1' expr2' evalState2
                                   

evalMiniScheme program = do (expr:exprs) <- parseMiniScheme program
                            return $ evalExpr expr (EvalState [] void)
                            
void :: Expression                           
void = List []