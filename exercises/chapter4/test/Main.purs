module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Path (root)
import Data.Foldable (for_)
import FileOperations (allFiles, biggestAndSmallest, whereIs)

main :: Eff ( console :: CONSOLE) Unit
main = do
  for_ (allFiles root) logShow
  logShow $ biggestAndSmallest root
  logShow $ whereIs "/bin/ls"

