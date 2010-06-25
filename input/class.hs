data D = D1 | D2

class ToD a where
    toInt :: a -> Int
    toD :: a -> D
    toD x = D1

-- class ToD a => ToD2 a where
--     toD2 :: a -> D
--     toD2 x = bar x

instance ToD a => ToD [a]

instance ToD Int where
    toInt x = x + 1

-- instance (Foo [a]) => (Foo (a, b)) where
--     foo (x, y) = foo x

-- test x = foo (quux x)

--testInstanceDirect :: Int -> Int
testInstanceDirect 1 = toInt 1

-- testInherit :: ToD2 a => a -> Int
-- testInherit x = toInt x

-- testFoo x = toInt x
            
