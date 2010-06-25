class Foo a
    
forceFoo :: Foo a => a -> a
forceFoo x = x

testInherit x = forceFoo x
