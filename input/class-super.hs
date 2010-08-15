undefined = undefined

(||) :: Bool -> Bool -> Bool
(||) = undefined

(&&) :: Bool -> Bool -> Bool
(&&) = undefined

not True = False
not False = True
            
class Eq a => Ord a where
    (<) :: a -> a -> Bool
    (>) :: a -> a -> Bool 
              
class Eq a where
    (==) :: a -> a -> Bool
    -- x == y = not (x /= y)             
    (/=) :: a -> a -> Bool
    -- x /= y = not (x == y)
           
eqInt :: Int -> Int -> Bool
eqInt = undefined
           
instance Eq Int where
    (==) = eqInt
    x /= y = not (x == y)

instance Eq e => Eq [e] where
    [] == [] = True
    (x:xs) == (y:ys) = (x == y) && (xs == ys)
    _ == _ = False

instance Ord a => Ord [a] where
    [] < [] = False
    [] < _ = True
    _ < [] = False
    (x:xs) < (y:ys) = (x < y) || ((x == y) && (xs < ys))

-- elem :: Eq a => a -> [a] -> Bool
-- elem :: e -> [e] -> Bool
elem x [] = False
elem x (y:ys) = (x == y) || (elem x ys)

test = elem "foo"                  

foo x y = (x < y) || (x == y)       
