default ()

data Foo = Foo        
        
undef = undef        
        
genericLength :: Num a => [b] -> a
genericLength = undef

g xs = let x = 1
       in (x, x)
                
f xs = let len = genericLength xs
       in (len,len)
        
