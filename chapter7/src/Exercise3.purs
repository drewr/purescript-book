module Exercise3 where

import Prelude

data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance traversableTree :: Traversable (Tree a) where
  traverse
