module Exercise1 where

import Prelude
import Data.Array (head, tail)
import Data.Maybe (Maybe(..))
import Data.List

third :: forall a. Array a -> Maybe a
third xs = do
  ys <- tail xs
  zs <- tail ys
  z <- head zs
  pure z

-- > :r
-- Compiling Exercise1
-- > import Prelude
-- > import Data.Maybe
-- > Just 1
-- (Just 1)

-- > :t (<*>)
-- forall a b f. (Apply f) => f (a -> b) -> f a -> f b

-- > :t apply
-- forall a b f. (Apply f) => f (a -> b) -> f a -> f b

-- > :t ap
-- forall m a b. (Monad m) => m (a -> b) -> m a -> m b

-- > ap (Just (\x -> x + 1)) (Just 1)
-- (Just 2)

-- > apply (Just (\x -> x + 1)) (Just 1)
-- (Just 2)

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM f (x:xs) = do
  b <- f x
  xs' <- filterM f xs
  pure if b then x : xs' else xs'
