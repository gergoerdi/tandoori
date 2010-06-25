class Foo a
instance Foo Int
instance Foo a => Foo [a]    

forceFoo :: Foo a => a -> a
forceFoo x = x

testInherit = forceFoo 1

testInheritCompound = forceFoo []

testCompund [] = forceFoo []
testCompund xs = forceFoo xs
