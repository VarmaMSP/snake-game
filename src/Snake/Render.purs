module Snake.Render where

import Prelude
import Control.Monad.Eff (Eff, foreachE)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import DOM (DOM)
import Graphics.Canvas (getCanvasElementById, getContext2D, clearRect, setFillStyle, fillRect)
import Partial.Unsafe (unsafePartial)
import Snake.Types (Model, blockSize, boardSize, ref)

foreign import setScore :: forall eff. Int -> Eff (dom :: DOM | eff) Unit

render :: Model -> Eff _ Unit
render { snake, food, t, score } = void $ unsafePartial $ do
  (Just canvas) <- getCanvasElementById "snake-board"
  context <- getContext2D canvas
  let white = "#FFFFFF"
      black = "#000000"

  -- set score
  setScore score
  -- clear canvas
  _ <- clearRect context
    { x : 0.0
    , y : 0.0
    , w : blockSize * boardSize + 2.0 * ref
    , h : blockSize * boardSize + 2.0 * ref
    }
  -- paint board's boundary
  _ <- setFillStyle black context
  _ <- fillRect context
    { x : 0.0
    , y : 0.0
    , w : blockSize * boardSize + 2.0 * ref
    , h : blockSize * boardSize + 2.0 * ref
    }
  _ <- setFillStyle white context
  _ <- fillRect context
    { x : ref
    , y : ref
    , w : blockSize * boardSize
    , h : blockSize * boardSize
    }
  -- paint food
  _ <- setFillStyle black context
  _ <- fillRect context
    { x : blockSize * (toNumber $ fst food) + ref + 1.0 + (toNumber t)
    , y : blockSize * (toNumber $ snd food) + ref + 1.0 + (toNumber t)
    , w : blockSize - 2.0 - (2.0 * (toNumber t))
    , h : blockSize - 2.0 - (2.0 * (toNumber t))
    }
  -- paint snake
  foreachE snake (\(Tuple p q) -> void $ do
    _ <- setFillStyle black context
    fillRect context
      { x : blockSize * (toNumber p) + ref + 1.0
      , y : blockSize * (toNumber q) + ref + 1.0
      , w : blockSize - 2.0
      , h : blockSize - 2.0
      }
  )
