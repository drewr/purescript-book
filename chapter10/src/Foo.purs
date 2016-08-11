module Foo where

import Prelude
import Data.Foreign (F, readString, toForeign)
import Data.Foreign.Class (class IsForeign, readJSON, read, readProp)
import Data.Foreign.Index (prop)
import Data.Foreign.Null (unNull)

newtype FormData = FormData
  { firstName  :: String
  , lastName   :: String
  , age        :: Int
  }

instance formDataIsForeign :: IsForeign FormData where
  read value = do
    firstName   <- readProp "firstName" value
    lastName    <- readProp "lastName"  value
    age         <- readProp "age"       value
    pure $ FormData
      { firstName
      , lastName
      , age
      }

