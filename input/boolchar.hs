{-# LANGUAGE NoImplicitPrelude #-}
import Prelude (Bool, Char)

undefined = undefined

not :: Bool -> Bool
not = undefined

const :: a -> b -> a
const = undefined

toUpper :: Char -> Char
toUpper = undefined

foo x = (toUpper x, not x)
