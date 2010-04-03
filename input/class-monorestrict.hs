default ()        
        
genericLength :: Num a => [b] -> a
genericLength [] = 0
      
test xs = let len = genericLength xs
          --in (len, len)
          in len

len = genericLength []                   
-- test' = (len, len)
