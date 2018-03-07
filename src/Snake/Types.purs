module Snake.Types where

import Prelude
import Data.Tuple (Tuple)

type Point = Tuple Int Int
type Snake = Array Point
type Model = { snake :: Snake
             , food :: Point
             , direction :: Int
             , alive :: Boolean
             , t :: Int
             , score :: Int
             }

blockSize :: Number
blockSize = 20.0

boardSize :: Number
boardSize = 20.0

ref :: Number
ref = 5.0
