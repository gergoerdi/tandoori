list x = [x]

map f [] = []
map f (x:xs) = (f x):(map f xs)
