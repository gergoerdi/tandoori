data Either a b = Left a | Right b

-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

-- instance Functor [] where
--     fmap f [] = []
--     fmap f (x:xs) = (f x):(fmap f xs)

-- instance Functor ((,) a) where
--     fmap f (x, y) = (x, f y)

class C1 a where
    quux :: a -> Int
    
class C1 a => C2 a
class C2 a => C3 a

-- instance C1 Char
instance C2 Char -- Works because of C1 Char

instance C2 Int    
instance C1 Int
    
instance C1 a => C1 [a]
    
instance C2 a => C2 [a] -- Works because C2 a ensures C1 a, which in turn ensures C1 [a]
-- instance (C1 a, C2 a) => C3 a

instance (C1 a, C1 b) => C1 (a, b)

--instance C1 (Maybe Int)    


--instance C2 ([] a)

-- foo :: C1 a => a -> a
-- foo = undefined
       
-- bar1 x = quux ([[x]], "foo")
bar1 x = quux ("foo", "bar")
