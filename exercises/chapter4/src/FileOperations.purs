module FileOperations where

import Prelude

import Data.Array (concatMap, filter, foldl, (:))
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))
import Data.Path (Path(..), ls, isDirectory, size)
import Data.Newtype (class Newtype, unwrap, wrap)

newtype Acc = Acc
              { biggest  :: Path
              , smallest :: Path
              }

derive instance newtypeAcc :: Newtype Acc _
derive instance genericAcc :: Generic Acc

instance showAcc :: Show Acc where
  show = gShow

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles = allFiles >>> filter (not isDirectory)

biggestAndSmallest :: Path -> Acc
biggestAndSmallest path =
  foldl (\acc p ->
          let { biggest: currentBiggest
              , smallest: currentSmallest
              } = unwrap acc
          in Acc
             {
               biggest: if size p > size currentBiggest
                        then p
                        else currentBiggest
             , smallest: if size p < size currentSmallest
                         then p
                         else currentSmallest
             })

        (Acc {
          biggest: (File ".start" 0)
        , smallest: (File ".start" 9999999999)
        })
        (onlyFiles path)
