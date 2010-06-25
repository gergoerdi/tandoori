-- data FooType = Baz
data TestType a = Foo a | Bar a FooType Bool

-- foo :: TestType a
-- foo = undefined           
      
bar (Foo x) | x == 1 = Foo 1
