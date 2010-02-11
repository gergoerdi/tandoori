class Foo a where
    foo :: a -> Int
    bar :: a -> Int
    bar x = foo x

instance Foo Int where
    foo x = x

instance (Foo [a]) => (Foo (a, b)) where
    foo (x, y) = foo x
