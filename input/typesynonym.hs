type ShowS = String -> String
    
myShow :: String -> ShowS
myShow s = \x -> s
