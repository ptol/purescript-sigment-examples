module Main where

import Prelude
import Sigment
import Data.Array
import Data.Array.Unsafe as U
import Data.Maybe
import Data.Maybe.Unsafe
import Control.Monad.Eff
import Data.Tuple
import DOM.Timer as Timer
import Data.Foldable
import Data.Traversable
import Control.Monad.Eff.Random
import Sigment.Dom as D
import Sigment.Dom.Props as P
import Sigment.Dom.Tweens as T
import Control.Apply

foodCount = 50
cellCount = 50
cellSize = 10

data Action = Move | ChangeDirection Int

type Position = Tuple Int Int

type Snake = Array Position

type Model = {
  snake :: Snake,
  food :: Array Position,
  direction :: Int
}

init :: Eff _ Model
init = do
  food <- generateFoodPositions
  pure {
    snake : [Tuple (cellCount / 2) (cellCount / 2)],
    food : food,
    direction : 0
  }

directions = [
  Tuple 0 (-1),
  Tuple 1 0,
  Tuple 0 1,
  Tuple (-1) 0
]

generateFoodPositions = 1 .. foodCount # traverse (const randomPosition)

randomPosition = do
  x <- randomIndex cellCount
  y <- randomIndex cellCount
  pure $ Tuple x y
  where
    randomIndex count = randomInt 0 (count - 1)

isOut x size = x < 0 || x >= size

positionIsOut :: Position -> Boolean
positionIsOut (Tuple x y) = isOut x cellCount || isOut y cellCount

gameOver :: Model -> Boolean
gameOver state =
  positionIsOut head || elem head (U.tail state.snake)
  where
    head = U.head state.snake



changePosition directionIndex pos = Tuple (fst pos + fst direction) (snd pos + snd direction)
  where
    direction = U.unsafeIndex directions directionIndex

eval :: Eval Action Model _
eval (ChangeDirection direction) state dispatch = do
  pure $ state {direction = direction}
eval Move state dispatch = do
  Timer.timeout 100 (dispatch Move) *> pure unit
  if gameOver newState then init else pure newState
  where
    head = state.snake # U.head
    newHeadPosition = changePosition state.direction head
    snake = if elem newHeadPosition state.food then newHeadPosition : state.snake else newHeadPosition : U.init state.snake
    food = delete newHeadPosition state.food
    newState = state {snake = snake, food = food}

snakeSprite = "assets/snake.png"
foodSprite = "assets/food.png"

directionKeys = [Tuple "up" 0, Tuple "right" 1, Tuple "down" 2, Tuple "left" 3]
render :: Render Action Model _
render action state dispatch =
  D.group' [P.keyboard keys] [food, snake]
  where
    keys = directionKeys <#> (\(Tuple key direction) -> P.newKeys key (dispatch (ChangeDirection direction)))
    snake = D.group $ state.snake <#> (\(Tuple x y) -> D.sprite [P.src snakeSprite, P.x $ x * cellSize, P.y $ y * cellSize])
    food = D.group $ state.food <#> (\(Tuple x y) -> D.sprite [P.src foodSprite, P.x $ x * cellSize, P.y $ y * cellSize])

component :: Component Unit Action Model _
component = newComponent (const init) eval render

main = do
  let size = cellCount * cellSize
  let config = defConfig {
        sprites = [snakeSprite, foodSprite],
        containerId = "container",
        height = size,
        width = size,
        initAction = Just Move}
  Sigment.init config unit component
