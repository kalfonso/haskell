module MiniSchemeEvaluator (evalMiniScheme') where

import MiniSchemeParser
import Control.Monad.State

type Name = String

type Environment = [(Name,Expression)]

type EvalState = State Environment Expression

evalExpr :: Expression -> EvalState
evalExpr (Atom x) = evalAtom x
evalExpr (List [Atom "quote", expr]) = evalQuote expr
evalExpr (List [Atom "atom", expr]) = evalIsAtom expr
evalExpr (List [Atom "set", Atom var, value]) = state $ \env -> (voidExpr, ((var, value):env))
evalExpr (List [Atom "eq", expr1, expr2]) = evalEq expr1 expr2
evalExpr (List [Atom "cons", expr1, expr2]) = evalCons expr1 expr2
evalExpr (List [Atom "car", expr1]) = evalCar expr1
evalExpr (List [Atom "cdr", expr1]) = evalCdr expr1
evalExpr (List ((Atom "cond"):predicates)) = evalCond predicates
evalExpr (List ((List [Atom "lambda", List params, bodyExpr]):args)) = evalLambda params bodyExpr args
{--evalExpr (List ((Atom "quote"):(List [Atom "lambda", List params, bodyExpr]):args)) evalState = evalLambda params bodyExpr args evalState
evalExpr (List ((Atom functionName):args)) evalState = evalFunction functionName args evalState --}
evalExpr expr = error $ show expr 

evalAtom :: String -> EvalState
evalAtom x = do env <- get
                return $ lookupEnv x env

evalQuote :: Expression -> EvalState
evalQuote expr = state $ \env -> let quotedExpr = quoteList [expr]
                                 in (quotedExpr, env)

evalIsAtom :: Expression -> EvalState
evalIsAtom (List [Atom "quote",Atom _]) = return $ Bool True
evalIsAtom (List [Atom "quote",Bool _]) = return $ Bool True
evalIsAtom (List [Atom "quote",Int _]) = return $ Bool True
evalIsAtom (List [Atom "quote",_]) = return $ Bool False
evalIsAtom expr = do resultExpr <- evalExpr expr
                     evalIsAtom resultExpr

evalEq :: Expression -> Expression -> EvalState
evalEq (List [Atom "quote", Atom x]) (List [Atom "quote", Atom y]) = return $ Bool $ x == y
evalEq (List [Atom "quote", Int x]) (List [Atom "quote", Int y]) = return $ Bool $ x == y
evalEq (List [Atom "quote", List []]) (List [Atom "quote", List[]]) = return $ Bool True                                                                                          
evalEq (List [Atom "quote", _]) (List [Atom "quote", _]) = return $ Bool False
evalEq expr1 expr2 = do expr1' <- evalExpr expr1
                        expr2' <- evalExpr expr2                        
                        evalEq expr1' expr2'
                                   
evalCons :: Expression -> Expression -> EvalState
evalCons (List [Atom "quote", Atom x]) (List [Atom "quote", List xs]) = return $ List [Atom "quote", List ((Atom x):xs)]
evalCons (List [Atom "quote", Int x]) (List [Atom "quote", List xs]) = return $ List [Atom "quote", List ((Int x):xs)]
evalCons (List [Atom "quote", List _]) _ = error $ "'car' requires first argument to be an atom"
evalCons _ (List [Atom "quote", Atom _]) = error $ "'car' requires second argument to be a list"
evalCons expr1 expr2 = do expr1' <- evalExpr expr1
                          expr2' <- evalExpr expr2
                          evalCons expr1' expr2'
                                    
evalCar :: Expression -> EvalState
evalCar (List [Atom "quote", List xs]) = return $ quoteList $ [head xs]
evalCar (List [Atom "quote", _]) = error "'car' requires a list argument"
evalCar expr = do expr' <- evalExpr expr
                  evalCar expr'
                             
evalCdr :: Expression -> EvalState
evalCdr (List [Atom "quote", List xs]) = return $ quoteList $ tail xs
evalCdr (List [Atom "quote", _]) = error "'cdr' requires a list argument"
evalCdr expr = do expr' <- evalExpr expr
                  evalCdr expr'
                             
evalCond :: [Expression] -> EvalState
evalCond [] = error "Wrong 'cond' expression. One of the predicates must evaluate to '#t'"
evalCond ((List [predicate, resultExpr]):condExprs) = do expr <- evalExpr predicate
                                                         case expr of
                                                           Bool True -> evalExpr resultExpr
                                                           _ -> evalCond condExprs
                                                                  
evalLambda :: [Expression] -> Expression -> [Expression] -> EvalState
evalLambda params bodyExpr args = do argsExprs <- mapM (\arg -> evalExpr arg) args
                                     put $ buildEnv params argsExprs
                                     evalExpr bodyExpr
                                                                 
{--
evalFunction :: String -> [Expression] -> EvalState -> EvalState                                                                 
evalFunction functionName args evalState@(EvalState env _) = let (List lambdaExpr) = lookupEnv functionName env
                                                                 lambdaApplic = List (lambdaExpr ++ args)
                                                                 (EvalState _ expr) = evalExpr lambdaApplic evalState
                                                             in  (EvalState env expr)

evalMiniScheme program = do (expr:exprs) <- parseMiniScheme program
                            return $ evalExpr expr (EvalState [] void)

--}
                            
lookupEnv :: String -> Environment -> Expression                            
lookupEnv var [] = error $ "Variable not defined: " ++ var 
lookupEnv var ((var',expr):entries) | var == var' = expr
                                    | otherwise = lookupEnv var entries
                       
initialState :: (Environment, Expression)
initialState = ([], voidExpr)

evalExpressions :: [Expression] -> State Environment Expression
evalExpressions [expr] = evalExpr expr
evalExpressions (expr:exprs) = do evalExpr expr
                                  evalExpressions exprs
                            
evalMiniScheme' program = do exprs <- parseMiniScheme program                            
                             return $ evalState (evalExpressions exprs) []
                                                  
voidExpr :: Expression                           
voidExpr = List []

quoteList :: [Expression] -> Expression
quoteList exprs = List ((Atom "quote"):exprs)

buildEnv :: [Expression] -> [Expression] -> Environment
buildEnv [] [] = []
buildEnv ((Atom param):params) (expr:exprs) = ((param, expr) : (buildEnv params exprs))