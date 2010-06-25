undefined = undefined            

-- Library
head :: [a] -> a
head (x:xs) = x

tail :: [a] -> [a]
tail (x:xs) = xs

append :: [a] -> [a] -> [a]
append _ _ = []

-- The example from the paper
reverse [] = []
reverse (x:xs) = append (reverse xs) x
                 
last xs = head (reverse xs)
-- init xs = reverse (tail (reverse xs))

-- rotateR xs = last xs : init xs          
