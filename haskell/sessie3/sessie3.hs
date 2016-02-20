import Data.List  
data Tree a = Tip | Node a (Tree a) (Tree a)  deriving (Show)
instance  Ord a => Eq (Tree a) where
	a== b =  sort (tree2list a ) == sort (tree2list b) 



maptree :: (a -> b) -> Tree a -> Tree b
maptree f a = fmap f a  

instance Functor Tree where
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)
  fmap f Tip = Tip

tree2list::Tree t -> [t]
tree2list Tip = []
tree2list (Node a b c) = [a] ++ tree2list b ++ tree2list c

vouwtree::(b -> b -> b) -> b -> Tree b -> b
vouwtree op end boom =  vouw op end (tree2list boom )


vouw:: (t -> t -> t) -> t -> [t] -> t
vouw _ end [] = end
vouw op end (x:xs) = op x (vouw op end xs)


--tree2listbe::Tree a -> [a]
--tree2listbe Tip = []
--tree2list (Node a b c) = 





class Sequentie a where
	vorige :: a -> a  -- GEEN HAAKJES ROND vorige
	volgende :: a -> a
	
instance Sequentie Int   where
	vorige a =  pred a
	volgende a = succ a
	
instance Sequentie Char   where
	
	vorige a =  pred a
	volgende 'z' = error "geen volgende voor z"
	volgende a = succ a
	
	
instance Sequentie Bool   where
	vorige a =  not a
	volgende a = not a
	
	
class Sequentie a => LinksBegrensdeSequentie a where
	eerste :: a 
instance LinksBegrensdeSequentie Int where
   	eerste = 0
instance LinksBegrensdeSequentie Char where
 	eerste = 'a' 
 	
 	
 	
data PrologData a = String [PrologData] | String


	

	

