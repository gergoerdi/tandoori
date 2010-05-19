list x = [x]

map f [] = []
map f (x:xs) = (f x):(map f xs)

succ :: Int -> Int               
succ x = undefined
               
ones = 1:ones
nats = 1:map succ nats
