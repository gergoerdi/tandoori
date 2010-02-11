forceNum :: Num a => a -> a
forceNum x = x

sink :: Num a => a -> Bool
sink _ = True
             
testInherit = forceNum 1
              
testSink = sink testInherit
