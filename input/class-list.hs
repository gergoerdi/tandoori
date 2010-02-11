undefined = undefined

map f [] = []
map f (x:xs) = (f x):(map f xs)

(==) :: Eq a => a -> a -> Bool
(==) = undefined
        
(+) :: Num a => a -> a -> a        
(+) = undefined
       
testEq = map (\ x -> x == x)
testNum = map (\ x -> x + 1)
