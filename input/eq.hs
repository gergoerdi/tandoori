class Eq a where
    (==) :: a -> a -> Bool
    x == y = not (x /= y)
            
    (/=) :: a -> a -> Bool
    x /= y = not (x == y)
    
infix 4 ==
infix 4 /=


(&&) :: Bool -> Bool -> Bool    
(||) :: Bool -> Bool -> Bool
       
-- listEq :: [a] -> [a] -> Bool      
-- listEq [] [] = True
-- listEq (x:xs) (y:ys) = (x == y) && (listEq xs ys)

member x [] = False
member x (y:ys) = (x == y) || member x ys

instance Eq Bool where
    True  == True  = True
    False == False = True
    _     == _     = False

test = member True [True, False, False]                     
