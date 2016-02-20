-- Li Quan
-- 1e master computerwetenschappen optie ai

-- genereer de volgende rij, gegeven de vorige rij
volgendeRij :: [Int] -> [Int]
volgendeRij vorigeRij = zipWith (+) ([0] ++ vorigeRij) (vorigeRij ++ [0])
 
-- maak de driehoek van pascal beginnend bij de eerste rij
pascal :: [[Int]]
pascal = iterate volgendeRij [1]
