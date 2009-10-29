module Text.PrettyPrint.Tabulator(Cell, Tabular, column, (<$|$>), render, fromRows) where

import Data.List (maximumBy)    
import Data.Function (on)
    
type Cell = String
data Tabular = Column [Cell]
             | Beside Tabular Int Tabular
               
instance Show Tabular where
    show = unlines . render
               
t <$|$> t' = Beside t 1 t'
            
column cs = Column cs

width (Column cs)   = maximum' $ map length cs
    where maximum' [] = 0
          maximum' ns = maximum ns
width (Beside t sep t') = (width t) + sep + (width t')

pad :: Int -> String -> String                          
pad 0 s      = s
pad n []     = replicate n ' '
pad n (s:ss) = s:(pad (n-1) ss)
       
render :: Tabular -> [String]
render (Column cs)       = cs
render (Beside l sep r) = map (uncurry (++)) $ zip' llines rlines
    where wl = width l
          wr = width r
          llines = map (pad (wl + sep)) (render l)
          rlines = map (pad wr) (render r)

          zip' (x:xs) (y:ys) = (x, y):(zip' xs ys)
          zip' xs     []     = map (\ x -> (x, "")) xs
          zip' []     ys     = map (\ y -> (spaces, y)) ys
              where spaces = replicate wl ' '

fromPairs pairs = (column $ map fst pairs) <$|$> (column $ map snd pairs)
fromPairs3 pairs = (column $ map fst3 pairs) <$|$> (column $ map snd3 pairs) <$|$> (column $ map trd3 pairs)
    where fst3 (a, b, c) = a
          snd3 (a, b, c) = b
          trd3 (a, b, c) = c

fromRows rows = foldl (<$|$>) (column []) columns
    where columns = map (\ col -> column $ map (nth col) rows) [0..col_num]
          col_num = maximum $ map length rows
          nth _     []     = ""
          nth 0     (s:ss) = s
          nth (n+1) (s:ss) = nth n ss
