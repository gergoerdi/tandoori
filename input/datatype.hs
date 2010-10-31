data List' a = Nil' | Cons' a (List' a)

myMap f Nil' = Nil'
myMap f (Cons' x xs) = Cons' (f x) (myMap f xs)

-- f Nil' = Nil'
-- tail (Cons' x xs) = xs

foo = let x = []
          y = []
      in (1:x, 'a':y)

map f [] = []
map f (x:xs) = (f x):(map f xs)

idInt :: Int -> Int
idInt x = x

idBool :: Bool -> Bool
idBool x = x

test = (myMap, myMap)
test2 = let (m1, m2) = test
        in (m1 idInt, m2 idBool)
