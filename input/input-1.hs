feo = let head (x:xs) = x
          head (x:xs) = xs
      in head

foo = let id x = x
      in (id, id)
         
ffo = let fst (x:xs) = x
          fst [] = 0
      in fst

fdo = let zig = 1:zag
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
