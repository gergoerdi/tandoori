forceNum :: Num a => a -> a
forceNum x = x

testInherit x = forceNum x
