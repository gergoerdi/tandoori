foo = let x = 5
      in let zig x = zag x
             zag y = zig x
         in zag
                 

fco = let fst (x:xs) = x
      in fst 5

fbo = \ x -> y

fbo = let head (x:xs) = x
          head (x:xs) = xs
      in head

fao = let id x = x
      in (id, id)

fzo = let fst (x, y) = x
          fst [] = 0
      in fst

fco =let fst1 (x, y) = 1
         fst2 [] = 0
     in (fst1, fst2)
         
fbo = let fst (x:xs) = x
          fst [] = 0
      in fst

fbo = let zig = 1:zag
          zag = 2:zig
      in zig

fbo = let (a, b) = ([], [])
          ones = 1:ones
      in ones

fco = let (a, b) = (5, "foo")
      in (b, a)
         
fbo = let (a, b) = (5, "foo")
      in a

fao = let y = 5
      in \ x -> [x, x, (\ x -> x)]
