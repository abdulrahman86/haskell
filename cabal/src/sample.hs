


module Sample ( fringe1 ) where
	
	import Tree

	fringe1 :: Tree a -> [a]
	fringe1 (Leaf x)            = [x]
	fringe1 (Branch left right) = fringe1 left ++ fringe1 right