forceNum :: Num a => a -> a
forceNum x = x

testInherit = forceNum 1

testInheritCompound = forceNum []

testCompund [] = forceNum []
testCompund xs = forceNum xs
