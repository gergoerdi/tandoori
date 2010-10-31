undefined = undefined

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs) = if p x then filter p xs else x:filter p xs

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

not :: Bool -> Bool
not = undefined

toUpper :: Char -> Char
toUpper = undefined

f xs = let xform f = map f xs
       in (xform not, xform toUpper)
