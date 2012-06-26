module MiniScheme (ms) where

import Control.Monad.State

import MiniSchemeEvaluator

ms filePath = do program <- readFile filePath
                 return $ eval program


