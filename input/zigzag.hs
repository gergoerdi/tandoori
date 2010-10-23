fst (x, y) = x
snd (x, y) = y

data BalancedTree a = Zero a | Succ (BalancedTree (a,a))

zig :: BalancedTree a -> a
zig (Zero a) = a
zig (Succ t) = fst (zag t)

zag (Zero a) = a
zag (Succ t) = snd (zig t)
