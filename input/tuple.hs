id x = x
        
idPair x = (id x, id x)

class Funktor f where
    fmap :: (a -> b) -> f a -> f b

instance Funktor ((,) a) where
    fmap f (x, y) = (x, f y)

fmap :: (b -> c) -> ((,) a) b -> ((,) a) c
fmap f (x, y) = (x, f y)
