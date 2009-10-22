data MyData a = Foo a | Bar a (MyData a) Int
data List' a = Nil' | Cons' a (List' a)
              
-- fun (Foo x) = Foo 1
-- fun (Bar x _ _) = Bar 1 (Foo 1) 2

myMap f Nil' = Nil'
myMap f (Cons' x xs) = Cons' (f x) (myMap f xs)
