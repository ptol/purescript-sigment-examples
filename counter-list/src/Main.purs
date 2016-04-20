module Main where

import Sigment
import Prelude
import Data.Tuple
import Data.Maybe
import Data.Array
import Data.Maybe.Unsafe
import Sigment.Dom as D
import Sigment.Dom.Utils as D
import Sigment.Dom.Props as P
import Sigment.Dom.Tweens as T
import Control.Monad.Eff
import Control.Bind
import Data.Map as M
import Data.List as L
import Data.Function
import Sigment.Subcomponents

type Counters = M.Map Int Counter.Model

type Model = {
  counters :: Counters,
  sorted :: Boolean,
  nextId :: Int
}

maxCount = 5

data Action = AC (Tuple Counter.Action Int) | AddCounter

initState :: Model
initState = {counters : M.empty, nextId : 1, sorted : false}

component :: Component Unit Action Model _
component = newComponent (const initState >>> pure) eval render

accessorCounter :: Accessor Int Action Model Counter.Action Counter.Model
accessorCounter = newAccessor unwrap wrap stateGet stateSet
  where
    unwrap (AC (Tuple x _)) = Just x
    unwrap y = Nothing
    wrap key subA = AC (Tuple subA key)
    stateGet :: Int -> Model -> Maybe Counter.Model
    stateGet key s = M.lookup key s.counters
    stateSet :: Int -> Maybe Model -> Counter.Model -> Model
    stateSet key ms subS = let s = fromJust ms in s {counters = M.update (\_ -> Just subS) key s.counters}

subcomponentCounter :: SubcomponentWithKey Int Action Model _
subcomponentCounter = newSubcomponentWithKey Counter.component accessorCounter

eval :: Eval Action Model _
eval AddCounter state _ = pure $ state {counters = M.insert state.nextId 0 state.counters, nextId = state.nextId + 1}
eval (AC (Tuple Counter.Delete key)) state _ = pure $ state {counters = M.delete key state.counters}
eval action@(AC (Tuple _ key)) state dispatch = subcomponentCounter.eval key action state dispatch

defaultStyle = P.style {font: "30px Verdana", fill : "white"}

newCounter = T.creating [T.from [P.y 600], T.duration 500]
render :: Render Action Model _
render action state dispatch  = D.group' [P.x 100, P.y 50] [
  if length list == 5 then D.empty else D.text [P.txt "Add", defaultStyle, P.onClick $ dispatch AddCounter],
  counters
]
  where
    list :: Array _
    list = state.counters # M.toList # L.toUnfoldable
    counters = D.group' [P.y 70] $ list <#> (\(Tuple key x) ->
      D.group' [newCounter]
        [fromMaybe D.empty $ subcomponentCounter.render key action state dispatch]) # D.stackV 50

main = do
   Sigment.init (defConfig {sprites = [], width = 400, height = 400, containerId = "container"}) unit component
