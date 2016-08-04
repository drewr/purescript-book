module Exercise2 where

import Prelude
import Math (sqrt, pow)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, error)
import Control.Monad.ST (ST, newSTRef, readSTRef, modifySTRef)

safeDivide :: forall eff. Int -> Int -> Eff (err :: EXCEPTION | eff) (Maybe Int)
safeDivide _ 0 = throwException (error "no divide by zero")
safeDivide n m = pure $ Just (n / m)

type Successes = Int
type Attempts = Int

data Point = Point
 { x :: Number
 , y :: Number
 }

instance showPoint :: Show Point where
  show (Point {x, y}) = "(" <> show x <> "," <> show y <> ")"

makePoint :: forall eff. Eff ( random :: RANDOM | eff ) Point
makePoint = do
  rand1 <- random
  rand2 <- random
  pure $ Point { x: rand1, y: rand2 }

distance :: Point -> Point -> Number
distance (Point p1) (Point p2) = sqrt (dx + dy)
  where dx = pow (p2.x - p1.x) 2.0
        dy = pow (p2.y - p1.y) 2.0

pointInCircle :: Point -> Boolean
pointInCircle p = (distance p center) <= radius
  where center = Point { x: 0.5, y: 0.5 }
        radius = 0.5

calcPi :: Successes -> Attempts -> Number
calcPi succ att = 4.0 * ((toNumber succ) / (toNumber att))

main :: forall eff h. Eff ( console :: CONSOLE
                          , random :: RANDOM
                          , st :: ST h
                          | eff
                          )
                      Unit
main = do
  ref <- newSTRef { successful: 0, attempted: 0 }
  forE 0 3000000 $ \i -> do
    pt <- makePoint
    let inCircle = pointInCircle pt
    modifySTRef ref \o ->
      { successful: if inCircle then o.successful + 1 else o.successful
      , attempted: o.attempted + 1
      }
    tmp <- readSTRef ref
    let succ = tmp.successful
        att = tmp.attempted
        inOrOut = if inCircle then "IN" else "OUT"
        pi = calcPi succ att
    -- logShow $
    --   show i <> "(" <> inOrOut <> "): " <>
    --   show pt <> " att: " <> show att <> " succ: " <> show succ <> " pi: " <>
    --   show pi
    pure unit
  tmp <- readSTRef ref
  let succ = tmp.successful
      att = tmp.attempted
  logShow $ calcPi succ att
