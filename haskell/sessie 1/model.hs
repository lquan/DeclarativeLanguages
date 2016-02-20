-- 'eenvoudige' oplossing
pascal :: [[Int]]
pascal = pascalHulp [1] 
    where pascalHulp rij = rij : pascalHulp (1 : volgende rij) 

volgende :: [Int] -> [Int]
volgende [_] = [1]
volgende (a:b:rest) = (a+b) : volgende (b:rest)


-- met voorgedefinieerde functies 'iterate', 'zipWith' en een lambda-functie
pascalBI :: [[Int]]
pascalBI = iterate (\r -> zipWith (+) (0:r) (r++[0])) [1]