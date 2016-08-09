module AlertMain where

import Prelude
import Control.Monad.Eff.Alert (confirm)

main = do
  confirm "foo"
