module FileOperations where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concatMap, filter, foldl, null, (:))
import Partial.Unsafe (unsafePartial)
import Data.Array.Partial as DP
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Path (Path(..), ls, isDirectory, size, filename, root)

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

onlyDirectories :: Path -> Array Path
onlyDirectories = allFiles >>> filter isDirectory

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
        , smallest: (File ".start" 999999999)
        })
        (onlyFiles path)

whereIs :: String -> Maybe Path
whereIs s = do
  let x = dirsContainingMatch root s
  if not null x
    then Just (unsafePartial $ DP.last x)
    else Nothing
  where
    dirsContainingMatch :: Path -> String -> Array Path
    dirsContainingMatch p s = do
      d <- onlyDirectories p
      guard $ not null (filter (\f -> (filename f == s)) (ls d))
      pure d
