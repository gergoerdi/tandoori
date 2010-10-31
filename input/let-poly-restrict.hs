data Unit = Unit

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x:(xs ++ ys)

f xs = let g y = y : xs
       in g Unit ++ g True



filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs) = if p x then filter p xs else x:filter p xs
