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

leq  []      []      = True
leq  (x:xs)  []      = False
leq  []      (y:ys)  = False
leq  (x:xs)  (y:ys)  = (x == y) && (leq xs ys)

beq True   True   = True
beq False  False  = True
beq _      _      = False
