module Data.AddressBook.Validation where

import Prelude

import Data.AddressBook (Address(..), Person(..), PhoneNumber(..),
                         PhoneType(..), address, person, phoneNumber)
import Data.Either (Either(..))
import Data.String (length)
import Data.String.Regex (Regex, test, noFlags, regex)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, unV, invalid)
import Partial.Unsafe (unsafePartial)

type Errors = Array ValidationError

data ValidationError = ValidationError String Field

data Field = FirstNameField
           | LastNameField
           | StreetField
           | CityField
           | StateField
           | PhoneField PhoneType

instance showField :: Show Field where
  show FirstNameField = "first name"
  show LastNameField  = "last name"
  show StreetField    = "street"
  show CityField      = "city"
  show StateField     = "state"
  show (PhoneField o) = show o

instance showValidationError :: Show ValidationError where
  show (ValidationError msg field) = "Field '" <> show field <> "' " <> msg

nonEmpty :: Field -> String -> V Errors Unit
nonEmpty field "" = invalid [(ValidationError "must not be empty" field)]
nonEmpty _     _  = pure unit

arrayNonEmpty :: forall a. Field -> Array a -> V Errors Unit
arrayNonEmpty field [] = invalid [(ValidationError "must contain at least one value" field)]
arrayNonEmpty _     _  = pure unit

lengthIs :: Field -> Int -> String -> V Errors Unit
lengthIs field len value | length value /= len =
    invalid [(ValidationError ("must have length " <> show len) field)]
lengthIs _     _   _     = pure unit

phoneNumberRegex :: Regex
phoneNumberRegex =
  unsafePartial
    case regex "^\\d{3}-\\d{3}-\\d{4}$" noFlags of
      Right r -> r

matches :: Field -> Regex -> String -> V Errors Unit
matches _     regex value | test regex value = pure unit
matches field _     _     = invalid [(ValidationError "did not match the required format" field)]

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (nonEmpty StreetField o.street *> pure o.street)
          <*> (nonEmpty CityField   o.city   *> pure o.city)
          <*> (lengthIs StateField 2 o.state *> pure o.state)

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) =
  phoneNumber <$> pure o."type"
              <*> (matches (PhoneField o."type") phoneNumberRegex o.number *> pure o.number)

validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
  person <$> (nonEmpty FirstNameField o.firstName *> pure o.firstName)
         <*> (nonEmpty LastNameField  o.lastName  *> pure o.lastName)
         <*> validateAddress o.homeAddress
         <*> (arrayNonEmpty (PhoneField HomePhone) o.phones *> traverse validatePhoneNumber o.phones)

validatePerson' :: Person -> Either Errors Person
validatePerson' p = unV Left Right $ validatePerson p
