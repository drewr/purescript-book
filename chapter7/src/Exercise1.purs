module Exercise1 where

import Prelude (class Applicative, unit, pure, (<$>), (<*>), (*>), (+))
import Data.Maybe (Maybe(..))

plus :: Int -> Int -> Int
plus n m = n + m

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe (Just x) = pure <$> x
combineMaybe _ = pure Nothing
