module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Tuple (Tuple(..))
import FRP.Event (sampleOn_)
import FRP.Event.Time as Time
import FRP.Event.Keyboard as Keyboard
import FRP.Behavior (Behavior, unfold, animate)

import Snake.Types (Model)
import Snake.Update (update)
import Snake.Render (render)

snake :: Behavior Model
snake = unfold update event init
  where
    event = sampleOn_ Keyboard.down $ Time.interval 150
    init = { snake : [ Tuple 5 10, Tuple 5 11, Tuple 5 12 ]
           , food : Tuple 3 2
           , direction : 38
           , alive : true
           , t : 0
           , score : 0
           }

main :: Eff _ Unit
main = void $ animate snake render
