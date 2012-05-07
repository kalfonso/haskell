import Test.QuickCheck

class Equals a where
  eq :: a -> a -> Bool
  
instance Equals Integer where
  x `eq` y = x == y
