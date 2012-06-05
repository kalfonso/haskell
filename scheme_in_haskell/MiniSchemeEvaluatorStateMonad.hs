module MiniSchemeEvaluator (evalMiniScheme) where

import Control.Monad.State

type Environment = [(String, Int)]

setVar :: String -> Int -> State Environment ()
setVar name val = state $ \env -> ((), (name, val):env)

evalMiniScheme = error "Not implemented!"
