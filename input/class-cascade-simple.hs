undefined = undefined

class Foo a
    
forceFoo :: Foo a => a -> a
forceFoo x = undefined

testInherit x = forceFoo x

lam = \x -> forceFoo x
