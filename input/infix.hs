-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (.) f g x = f (g x)            

foo f g = (\ x -> g x)
            
f 1 = True
g True = []

-- testInfix = f . g         
