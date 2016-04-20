module Main where

import Prelude
import Sigment
import Sigment.Dom as D
import Sigment.Dom.Props as P
import Sigment.Dom.Tweens as T

data Action = Increase | Decrease

type Model = Int

initState = 0

component :: Component Unit Action Model _
component = newComponent (const initState >>> pure) (pureEval eval) render

eval :: PureEval Action Model
eval Increase = (_ + 1)
eval Decrease = (_ - 1)

defaultStyle = P.style {font: "30px Verdana", fill : "white"}
text str props = D.text $ [P.txt str, defaultStyle, P.anchorCenter] ++ props
countUpdate = T.updating $ [T.from [P.scale1 1.0], T.to [P.scale1 2.0], T.duration 300, T.easingCubic.out] ++ T.yoyoOnce
render :: Render Action Model _
render _ state dispatch = D.group' [P.x 120, P.y 200] [
  text "+" [P.onClick (dispatch Increase), P.x 30],
  text "-" [P.onClick (dispatch Decrease), P.x 60],
  text (show state) [P.key "count", P.x 120, countUpdate]
]

main = do
   init (defConfig {width = 400, height = 400, containerId = "container"}) unit component
