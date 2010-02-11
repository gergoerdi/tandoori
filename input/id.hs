id1 x = x
id2 x = x

data MyPair a b = MyPair a b        
        
idBoth x = MyPair (id1 x) (id2 x)
