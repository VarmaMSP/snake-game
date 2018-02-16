module Snake.Update where

import Prelude
import Data.Array (uncons, elem, dropEnd, (:))
import Data.Array as Array
import Data.Int (floor)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)
import Snake.Types (Model, Point, Snake, boardSize)

foreign import randomInt :: Int -> Int

update :: Int -> Model -> Model
update turn m
  | not (m.alive && (inBound m.snake)) = m { alive = false, t = (m.t + 1) `mod` 4 }
  | otherwise = moveSnake turn m

inBound :: Snake -> Boolean
inBound s = case uncons s of
  Just { head : h, tail : t } ->
    let x = fst h
        y = snd h
    in (x >= 0) && (y >= 0)
       && (x < (floor $ boardSize - 1.0))
       && (y < (floor $ boardSize - 1.0))
       && (not (h `elem` t))
  Nothing -> false

moveSnake :: Int -> Model -> Model
moveSnake turn m =
  let (Tuple d h) = nextHeadPos m.snake m.direction turn
      (Tuple f s) = if h == m.food
          then Tuple (nextFoodPos unit) (h : m.snake)
          else Tuple m.food (dropEnd 1 (h : m.snake))
      score' = if h == m.food
          then m.score + 10
          else m.score
  in m { snake = s, direction = d, food = f, t = (m.t + 1) `mod` 4, score = score' }

nextHeadPos :: Snake -> Int -> Int -> Tuple Int Point
nextHeadPos s dir turn =
  let h = unsafePartial $ fromJust $ Array.head s
      left  = Tuple (-1) 0
      right = Tuple 1 0
      up    = Tuple 0 (-1)
      down  = Tuple 0 1
  in case dir, turn of
    37, 39 -> Tuple 37 (h + left)
    38, 40 -> Tuple 38 (h + up)
    39, 37 -> Tuple 39 (h + right)
    40, 38 -> Tuple 40 (h + down)
    _, 37  -> Tuple 37 (h + left)
    _, 38  -> Tuple 38 (h + up)
    _, 39  -> Tuple 39 (h + right)
    _, 40  -> Tuple 40 (h + down)
    _, 32  -> nextHeadPos s dir dir
    x, _   -> nextHeadPos s dir dir

nextFoodPos :: Unit -> Point
nextFoodPos _ =
  let p = randomInt ((floor boardSize) - 1)
      q = randomInt ((floor boardSize) - 1)
  in Tuple p q
