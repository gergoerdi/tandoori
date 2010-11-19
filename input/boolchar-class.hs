{-# LANGUAGE NoImplicitPrelude #-}
import Prelude (Bool, Char)

class Foo a where
  foo :: a -> Bool

undefined = undefined

not :: Bool -> Bool
not = undefined

const :: a -> b -> a
const = undefined

toUpper :: Char -> Char
toUpper = undefined

test x = let y = foo x
         in not x
