map' :: (Int -> a) -> [Int] -> [a]
map' f [] = []
map' f (x:xs) = (f x):(map' f xs)

map f [] = []
map f (x:xs) = (f x):(map f xs)
                
id :: Int -> a
id 1 = 1
                
foo = let id :: String -> String
          id x = 1
      in map' id [1,2,3]

justfirst :: a -> b -> c
justfirst x y = x  
