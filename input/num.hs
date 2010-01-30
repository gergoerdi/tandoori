forceNum :: Num a => a -> a
forceNum x = x

forceEq :: Eq a => a -> a
forceEq x = x

forceBoth x = (forceNum x, forceEq x, forceNum (forceEq x))


              
forceList :: [a] -> [a]
forceList x = x

forceInt :: Int -> Int
forceInt x = x

forceString :: String -> String
forceString x = x


                
testList x = forceBoth (forceList x)
testInt x = forceBoth (forceInt x)
testString x = forceBoth (forceString x)

testDecl :: a -> (a, a)
testDecl = forceBoth
