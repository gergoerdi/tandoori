class Foo a => Quux a where
    quux :: a -> Int
    quux x = bar x

class Foo a where
    foo :: a -> Int
    bar :: a -> Int
    bar x = 1

instance Foo Int where
    foo x = x + 1

-- instance (Foo [a]) => (Foo (a, b)) where
--     foo (x, y) = foo x

-- test x = foo (quux x)

testInherit :: Quux a => a -> Int
testInherit x = foo x

testFoo x = foo x
            
