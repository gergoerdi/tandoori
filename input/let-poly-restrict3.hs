undefined = undefined

test x = let f y = x
             g y = x
         in (f, g)
