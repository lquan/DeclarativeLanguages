-- Li Quan
-- 1e master computerwetenschappen ai

{- tuple generation using Cantor diagonalization:
   generate all tuples that sum to K before the tuples that sum to K+1
   i.e., [(0,0),(0,1),(1,0),(0,2),(1,1),(2,0),(0,3),(1,2),(2,1),(3,0), ...]
-}
alle :: (Num t, Enum t) =>  (t -> t -> Bool) -> [(t,t)] 
alle f =  [ (y,x-y) | x <- [0..] , y <- [0..x], f y (x-y) ]

