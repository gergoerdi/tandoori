fgo = let fst (x:xs) = x
          fst [] = 0
      in fst

foo = let zig = 1:zag
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
