test x = let tick y = (x,y):tock y
             tock y = (x,y):tick y
         in (tick,tock)
            
test' x = let tick y = (x,y):tock y
              tock y = (y,x):tick y
          in (tick,tock)
