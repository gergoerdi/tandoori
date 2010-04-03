data List' a = Nil' | Cons' a (List' a)

myMap f Nil' = Nil'
myMap f (Cons' x xs) = Cons' (f x) (myMap f xs)

-- f Nil' = Nil'
-- tail (Cons' x xs) = xs
