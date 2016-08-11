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
  read value = makeFormData <$> readProp "firstName" value
                            <*> readProp "lastName"  value
                            <*> readProp "age"       value

instance showFormData :: Show FormData where
  show (FormData fd) = "FormData: " <> fd.lastName
                       <> ", " <> fd.firstName
                       <> " (" <> show fd.age <> ")"

makeFormData :: String -> String -> Int -> FormData
makeFormData fn ln ag = FormData { firstName: fn
                                 , lastName: ln
                                 , age: ag
                                 }
