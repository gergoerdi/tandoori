data Foo = Foo1 | Foo2
data Bar = Bar1 | Bar2
         
test = let v1 = Foo1
           v2 = Bar1
           (v3, v4) = (Foo2, Bar2)
       in (v1, v2, v3, v4)
