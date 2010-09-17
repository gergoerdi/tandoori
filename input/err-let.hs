test1 = let foo True = True
        in (foo 1, foo)

test2 = let empty = []
        in (empty, empty)

test3 = ([], [])           
