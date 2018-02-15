module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import FRP.Event.Keyboard as Keyboard
import FRP.Behavior (Behavior, unfold, animate)
import Graphics.Canvas (CANVAS, getCanvasElementById, getContext2D, clearRect, setFillStyle, fillPath, closePath, moveTo, lineTo)
import Math (sin, cos)

type Point  = { x :: Number, y :: Number }
type Square = { a :: Point, b :: Point, c :: Point, d :: Point }

square :: Behavior Square
square = unfold rotateSquare Keyboard.down init
  where
    init :: Square
    init =
      let l = 100.0
      in { a : { x : l , y : l  }
         , b : { x : -l, y : l  }
         , c : { x : -l, y : -l }
         , d : { x : l , y : -l }
         }

    rotateSquare :: Int -> Square -> Square
    rotateSquare d s =
      let dx = 0.07
      in case d of
        37 -> { a : rotate (-dx) s.a
              , b : rotate (-dx) s.b
              , c : rotate (-dx) s.c
              , d : rotate (-dx) s.d
              }
        39 -> { a : rotate dx s.a
              , b : rotate dx s.b
              , c : rotate dx s.c
              , d : rotate dx s.d
              }
        _ -> s

    rotate :: Number -> Point -> Point
    rotate angle p =
      { x :  p.x * cos angle + p.y * sin angle
      , y : -p.x * sin angle + p.y * cos angle
      }

render :: Square -> Eff _ Unit
render s = void $ unsafePartial $ do
  (Just canvas) <- getCanvasElementById "square"
  context <- getContext2D canvas
  let relativeToCanvas = (\p -> { x : 200.0 + p.x, y : 200.0 - p.y })
      a' = relativeToCanvas s.a
      b' = relativeToCanvas s.b
      c' = relativeToCanvas s.c
      d' = relativeToCanvas s.d

  _ <- clearRect context
      { x : 0.0
      , y : 0.0
      , h : 400.0
      , w : 400.0
      }

  _ <- setFillStyle "#0000FF" context
  fillPath context $ do
    _ <- moveTo context a'.x a'.y
    _ <- lineTo context b'.x b'.y
    _ <- lineTo context c'.x c'.y
    _ <- lineTo context d'.x d'.y
    closePath context

main :: Eff _ Unit
main = void $ animate square render
