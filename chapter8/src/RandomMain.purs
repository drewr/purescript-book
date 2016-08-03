module RandomMain where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Eff.Console (CONSOLE, logShow)

main :: forall eff. Eff ( random :: RANDOM
                        , console :: CONSOLE
                        | eff
                        )
                    Unit
main = do
  n <- random
  logShow n
