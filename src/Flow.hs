module Flow where

data Tree a = Node a [Tree a]

data Decision a b = Requirement (a -> Bool)
                  | Action (a -> b)


                  
