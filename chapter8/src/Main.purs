module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), examplePerson)
import Data.AddressBook.Validation (ValidationError(..), Errors, Field(..),
                                    hint, validatePerson')
import Data.Array ((..), length, modifyAt, zipWith, filter)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Foreign (F, readString, toForeign)
import Data.Foreign.Index (prop)
import Data.Maybe (fromJust, fromMaybe)
import Data.Nullable (toMaybe)
import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReadWrite, ReactState, Event, ReactThis, ReactElement,
              createFactory, readState, spec, createClass, writeState)
import React.DOM as D
import React.DOM.Props as P
import ReactDOM (render)

newtype AppState = AppState
  { person :: Person
  , errors :: Errors
  }

initialState :: AppState
initialState = AppState
  { person: examplePerson
  , errors: []
  }

valueOf :: Event -> F String
valueOf e = do
  target <- prop "target" (toForeign e)
  value <- prop "value" target
  readString value

updateAppState
  :: forall props eff
   . ReactThis props AppState
  -> (String -> Person)
  -> Event
  -> Eff ( console :: CONSOLE
         , state :: ReactState ReadWrite
         | eff
         ) Unit
updateAppState ctx update e =
  for_ (valueOf e) \s -> do
    let newPerson = update s

    log "Running validators"
    case validatePerson' newPerson of
      Left errors -> writeState ctx (AppState { person: newPerson, errors: errors })
      Right _ -> writeState ctx (AppState { person: newPerson, errors: [] })

addressBook :: forall props. ReactClass props
addressBook = createClass $ spec initialState \ctx -> do
  AppState { person: Person person@{ homeAddress: Address address }, errors } <- readState ctx

  let renderValidationError err =
        D.div [ P.className "alert alert-danger" ] [ D.text (show err) ]

      renderValidationErrors :: Field -> Errors -> Array ReactElement
      renderValidationErrors field errs = map renderValidationError $ onlyMatchingValidationErrors field errs

      onlyMatchingValidationErrors :: Field -> Errors -> Errors
      onlyMatchingValidationErrors field = filter \(ValidationError msg field') -> field == field'

      fieldHasError :: Field -> Errors -> Boolean
      fieldHasError field errs = length (onlyMatchingValidationErrors field errs) > 0

      formField field value update =
        D.div [ P.className "form-group" ] (if (fieldHasError field errors)
                                               then els <> (renderValidationErrors field errors)
                                               else els)
        where els = [ D.label [ P.className "col-sm-2 control-label" ]
                              [ D.text (show field) ]
                    , D.div [ P.className "col-sm-3" ]
                      [ D.input [ P._type "text"
                                , P.className "form-control"
                                , P.placeholder (hint field)
                                , P.value value
                                , P.onChange (updateAppState ctx update)
                                ] []
                      ]
                    ]

      renderPhoneNumber (PhoneNumber phone) index =
        formField f phone.number \s ->
          Person $ person { phones = fromMaybe person.phones $ modifyAt index (updatePhoneNumber s) person.phones }
        where f = (PhoneField phone."type")

      updateFirstName s = Person $ person { firstName = s }
      updateLastName  s = Person $ person { lastName  = s }

      updateStreet s = Person $ person { homeAddress = Address $ address { street = s } }
      updateCity   s = Person $ person { homeAddress = Address $ address { city   = s } }
      updateState  s = Person $ person { homeAddress = Address $ address { state  = s } }

      updatePhoneNumber s (PhoneNumber o) = PhoneNumber $ o { number = s }

  pure $
    D.div [ P.className "container" ]
          [ D.div [ P.className "row" ]
                  [ D.form [ P.className "form-horizontal" ] $
                           [ D.h3' [ D.text "Basic Information" ]

                           , formField FirstNameField person.firstName updateFirstName
                           , formField LastNameField  person.lastName  updateLastName

                           , D.h3' [ D.text "Address" ]

                           , formField StreetField address.street updateStreet
                           , formField CityField   address.city   updateCity
                           , formField StateField  address.state  updateState

                           , D.h3' [ D.text "Contact Information" ]
                           ]
                           <> zipWith renderPhoneNumber person.phones (0 .. length person.phones)
                  ] ]

main :: Eff ( console :: CONSOLE
            , dom :: DOM
            ) Unit
main = void do
  log "Rendering address book component"
  let component = D.div [] [ createFactory addressBook unit ]
  doc <- window >>= document
  ctr <- getElementById (ElementId "main") (documentToNonElementParentNode (htmlDocumentToDocument doc))
  render component (unsafePartial fromJust (toMaybe ctr))
