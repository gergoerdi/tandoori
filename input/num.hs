-- numeric x = 1

-- foo = (numeric, numeric)

forceNum :: Num a => a -> a
forceNum x = x

forceEq :: Eq a => a -> a
forceEq x = x

forceBoth x = (forceNum x, forceEq x)
