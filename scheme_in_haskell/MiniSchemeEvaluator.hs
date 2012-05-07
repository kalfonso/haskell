module MiniSchemeEvaluator (evalMiniScheme) where

import MiniSchemeParser

type Name = String

type Environment = [(Name,Expression)]

data EvalState = EvalState Environment Expression deriving Show

evalExpr :: Expression -> EvalState -> EvalState
evalExpr (List [Atom "quote", expr]) evalState = evalQuote expr evalState
evalExpr (List [Atom "atom", expr]) evalState = evalIsAtom expr evalState
evalExpr expr _ = error $ show expr 

evalQuote :: Expression -> EvalState -> EvalState
evalQuote expr (EvalState env _) = EvalState env expr

evalIsAtom :: Expression -> EvalState -> EvalState
evalIsAtom (List [Atom "quote",Atom _]) (EvalState env _) = EvalState env $ Bool True
evalIsAtom (List [Atom "quote",Bool _]) (EvalState env _) = EvalState env $ Bool True
evalIsAtom (List [Atom "quote",Int _]) (EvalState env _) = EvalState env $ Bool True
evalIsAtom (List [Atom "quote",_]) (EvalState env _) = EvalState env $ Bool False
evalIsAtom expr evalState = evalExpr expr evalState

evalMiniScheme program = do (expr:exprs) <- parseMiniScheme program
                            return $ evalExpr expr (EvalState [] void)
                            
void :: Expression                           
void = List []