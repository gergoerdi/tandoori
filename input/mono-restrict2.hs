default ()

undef = undef        
        
forceNum :: Num a => a -> a
forceNum x = x

mono x = forceNum x
mono' = mono

genericLength :: Num a => [b] -> a
genericLength = undef

f xs = let len = genericLength xs
       in (len,len)
        
