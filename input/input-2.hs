data TestType = Foo | Bar

hello :: String
hello = 'H':"ello World!"

-- ziguser = zig        

zig :: [TestType]      
zig = (Foo:zag)

zag :: [TestType]
zag = Bar:zig
        
main = print hello
