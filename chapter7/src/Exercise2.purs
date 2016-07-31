module Exercise2 where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.AddressBook (Address(..), Person(..), PhoneNumber(..)
                        , address, person, phoneNumber)
import Data.AddressBook.Validation (matches)
import Data.String.Regex (Regex, noFlags, regex)
import Data.Validation.Semigroup (V, unV, invalid)
import Partial.Unsafe (unsafePartial)

type Errors = Array String

stateRegex :: Regex
stateRegex =
  unsafePartial
    case regex "^[A-Z]{2}$" noFlags of
      Right r -> r

emptyRegex :: Regex
emptyRegex =
  unsafePartial
    case regex ".*[^\\s]+.*" noFlags of
      Right r -> r

nonEmpty :: String -> String -> V Errors Unit
nonEmpty field s = matches field emptyRegex s

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (nonEmpty "Street" o.street *> pure o.street)
          <*> (nonEmpty "City"   o.city   *> pure o.city)
          <*> (matches "State" stateRegex o.state *> pure o.state)
