module Exercise1 where

import Prelude
import Data.Array (head, tail)
import Data.Maybe (Maybe(..))

third :: forall a. Array a -> Maybe a
third xs = do
  as <- tail xs
  bs <- tail as
  c <- head bs
  pure c
