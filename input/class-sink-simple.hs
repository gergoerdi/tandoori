{-# LANGUAGE NoMonomorphismRestriction #-}
class Show a where
    show :: a -> String

class Read a where
    read :: String -> a

readShow x = read (show x)
showRead s = show (read s)
showReadMono s = let y = read s
                 in (y, show y)

-- read' s = let y = read s in y
