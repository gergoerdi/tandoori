class Show a where
    show :: a -> String

class Read a where
    read :: String -> a

readShow x = read (show x)
showRead s = show (read s)
