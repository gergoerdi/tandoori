-- undefined = undefined

class Eq a where
    (==) :: a -> a -> Bool
    x /= y = not (x == y)
            
forceNum :: Eq a => a -> a
forceNum x = x

sink :: Eq a => a -> Bool
sink _ = undefined

testInherit = forceNum undefined

-- testInheritDecl :: Num a => a
-- testInheritDecl = forceNum 1
              
testSink = sink testInherit
-- testSink' = sink testInheritDecl
