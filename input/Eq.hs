data Bool = False | True                 

not :: Bool -> Bool
not = undefined
     
(&&) :: Bool -> Bool -> Bool
(&&) = undefined
       
(||) :: Bool -> Bool -> Bool
(||) = undefined
     
class Eq a where
    (==) :: a -> a -> Bool
    x == y = not (x /= y)
            
    (/=) :: a -> a -> Bool
    x /= y = not (x == y)
    
infix 4 ==
infix 4 /=

instance Eq a => Eq [a] where
    []      ==  []      = True
    (x:xs)  ==  []      = False
    []      ==  (y:ys)  = False
    (x:xs)  ==  (y:ys)  = x == y && xs == ys
