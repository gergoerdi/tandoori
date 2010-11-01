test x = let tick y = (x,y):tock y
             tock y = (x,y):tick y
         in (tick,tock)
