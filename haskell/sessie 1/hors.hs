laatste::[Int]->Int
laatste [x] = x
laatste (_:xs) = laatste xs

lineariseer::[[Int]]->[Int]
lineariseer [] = []
lineariseer [x] = x
lineariseer (x1:xs) = x1 ++ lineariseer xs  

herhaal::Int->Int->[Int]
herhaal 0 _ = []
herhaal n x = [x] ++ herhaal (n-1) x

bereik::Int->Int->[Int]
bereik a b | a > b = []
	   | a == b = [a]
	   | a < b = [a] ++ bereik (a+1) b
	  
verwijderVeelvouden::Int->[Int]->[Int]
verwijderVeelvouden n [x] = if  mod x n /= 0 then [x]
			    else []
verwijderVeelvouden n (x:xs) = 	(verwijderVeelvouden n [x]) ++ (verwijderVeelvouden n xs)

--vouwen
som::[Int] -> Int
som [] = 0
som (x:xs) = x + som xs


produkt::[Int] -> Int
produkt [] = 1
produkt (x:xs) = x * produkt xs

vouw:: (Int -> Int -> Int) -> Int -> [Int] -> Int
vouw _ end [] = end
vouw op end (x:xs) = op x (vouw op end xs)

zeef::Int->[Int]
zeef n = [x | x<-[2..n], zeef_hulp 

zeef_hulp::[Int]->[Int]
zeef_hulp (x:xs) =  


--priemgetallen :: [Int]    
--priemgetallen = zeef1 [2..]   
--   where zeef1 (p:xs) = p : zeef1 [x | x<-xs, x `mod` p /= 0]



                   
