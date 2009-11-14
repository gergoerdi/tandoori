foo = let id x = x
          const x y = x
          one = 1
          constone x = 1
          lamid = \ x -> x
      in (id, const, one, constone, lamid)
