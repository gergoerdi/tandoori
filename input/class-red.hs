undef = undef

eq :: Eq a => a -> a -> Bool
_ `eq` _ = undef
    
f xs y = xs `eq` [y]

f' :: (Monad m, Eq (m a)) => a -> m a -> Bool
f' x y = return x == y         
