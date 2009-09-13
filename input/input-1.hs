foo = let fst (x:xs) = x
      in fst 5

fbo = \ x -> y

fco = let head (x:xs) = x
          head (x:xs) = xs
      in head

fao = let id x = x
      in (id, id)

fao = let fst (x, y) = x
          fst [] = 0
      in fst
         
fao = let fst (x:xs) = x
          fst [] = 0
      in fst

fao = let zig = 1:zag
          zag = 2:zig
      in zig

fdo = let (a, b) = ([], [])
          ones = 1:ones
      in ones

fco = let (a, b) = (5, "foo")
      in (b, a)
         
fbo = let (a, b) = (5, "foo")
      in a

fao = let y = 5
      in \ x -> [x, x, (\ x -> x)]
