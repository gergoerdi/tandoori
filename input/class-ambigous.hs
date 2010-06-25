undef = undef

forceEq :: Eq a => a -> a
forceEq x = x

sink :: Eq a => a -> Bool
sink _ = undef

testInherit = sink (forceEq undef)

-- testInheritDecl :: Num a => a
-- testInheritDecl = forceNum 1
              
-- testSink = sink testInherit
-- testSink' = sink testInheritDecl
