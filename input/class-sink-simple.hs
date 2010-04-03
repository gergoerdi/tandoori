sink :: Eq a => a -> Bool
sink _ = undefined

testInherit :: Eq a => a         
testInherit = undefined

testSink = sink testInherit
