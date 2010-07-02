undefined = undefined

(||) :: Bool -> Bool -> Bool
(||) = undefined

(&&) :: Bool -> Bool -> Bool
(&&) = undefined
            
class Eq a => Ord a where
    (<) :: a -> a -> Bool
    (>) :: a -> a -> Bool 
              
class Eq a where
    (==) :: a -> a -> Bool

    (/=) :: a -> a -> Bool

eqInt :: Int -> Int -> Bool
eqInt = undefined
           
instance Eq Int where
    (==) = eqInt
    x /= y = not (x == y)

instance Eq e => Eq [e] where
    [] == [] = True
    (x:xs) == (y:ys) = x == y && xs == ys
    _ == _ = False

instance Ord a => Ord [a] where
    [] < [] = False
    [] < _ = True
    _ < [] = False
    (x:xs) < (y:ys) = x < y || (x == y && xs < ys)

-- member :: Eq a => a -> [a] -> Bool
-- member :: e -> [e] -> Bool
member x [] = False
member x (y:ys) = (x == y) || member x ys

memb = member 10                  
