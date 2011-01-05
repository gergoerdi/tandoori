{-# LANGUAGE NoImplicitPrelude #-}
import Prelude (Bool, Char)

class Eq a where
  (==) :: a -> a -> Bool

undefined = undefined

not :: Bool -> Bool
not = undefined

const :: a -> b -> a
const = undefined

toUpper :: Char -> Char
toUpper = undefined

test x = let y = x == x
         in not x
