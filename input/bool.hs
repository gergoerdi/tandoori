-- data Bool = True | False

ifthenelse :: Bool -> a -> a -> a
ifthenelse  True   x  y  = x
ifthenelse  False  x  y  = y
