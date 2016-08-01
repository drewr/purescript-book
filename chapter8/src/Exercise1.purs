module Exercise1 where

import Prelude
import Data.Array (head, tail)
import Data.Maybe (Maybe(..))

third :: forall a. Array a -> Maybe a
third xs = do
  ys <- tail xs
  zs <- tail ys
  z <- head zs
  pure z
