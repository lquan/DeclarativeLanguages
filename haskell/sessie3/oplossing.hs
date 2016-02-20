-- Li Quan
-- 1e master computerwetenschappen ai

-- enkel eerste deel afgewerkt
data Knoop a = D a | M (a->a) | Z (a->a->a) 

data Net a = Net { knopen :: [Knoop a],
		   bogen :: [(Int,Int)],
 		   index :: Int
 		 } 

--bereken :: Net a -> a
--bereken (Net knoop koppel n) = 


-- get (D d) = d
-- ...

geefInvoer :: Net t -> Int -> [(Int, Int)]
geefInvoer (Net _ b _) i = [(x,y) | (x,y) <- b, (y == i)]


