module Counter where

import Prelude
import Sigment
import Sigment.Dom as D
import Sigment.Dom.Props as P
import Sigment.Dom.Tweens as T

data Action = Increase | Decrease | Delete

type Model = Int

initState = 0

component :: Component Unit Action Model _
component = newComponent (const initState >>> pure) (pureEval eval) render

eval :: PureEval Action Model
eval Increase = (_ + 1)
eval Decrease = (_ - 1)
eval Delete = id

defaultStyle = P.style {font: "30px Verdana", fill : "white"}
text str props = D.text $ [P.txt str, defaultStyle, P.anchorCenter] ++ props
render :: Render Action Model _
render _ state dispatch = D.group [
  text "\215" [P.onClick (dispatch Delete), P.x 0],
  text "+" [P.onClick (dispatch Increase), P.x 30],
  text "-" [P.onClick (dispatch Decrease), P.x 60],
  D.thunk "value" (\x -> text (show x) [P.x 120]) state
]
