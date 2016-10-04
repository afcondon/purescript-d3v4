module Main where

import D3.Selection
import Control.Monad.Eff.Console (CONSOLE)
import D3.Base (DataBind(Keyed), AttrSetter(SetAttr), D3, Eff, (...), (..))
import D3.Transitions (TransitionName(..), duration, d3Transition)
import Data.String (toCharArray)
import Prelude (Unit, unit, pure, bind, (-), (<>), (/), show)

-- define a margin, look to purescript-css for more sophisticated definition
margin :: { top::Number, right::Number, bottom::Number, left::Number }
margin = { top: 20.0, right: 20.0, bottom: 30.0, left: 40.0}

update :: forall d eff. Array d -> Selection d -> Eff (d3::D3|eff) Unit
update d g = do
  -- | set up the transition that we'll use between phases
  t <- d3Transition (Name "t")
    .. duration 750.0

  -- || JOIN new data with old elements
  text <- g ... selectAll "text"
    .. dataBind (Keyed d (\d -> d))

  -- || EXIT old elements not present in new data
  -- || UPDATE old elements present in new data
  -- || ENTER new elements present in new data

  pure unit

alphabet :: Array Char
alphabet = toCharArray "abcdefghijklmnopqrstuvwxyz"

main :: ∀ e. Eff (d3::D3,console::CONSOLE|e) Unit
main = do
-- general setup
  svg <- d3Select ".svg"
  w   <- svg ... getAttr "width"
  h   <- svg ... getAttr "height"
  let width =  w - margin.left - margin.right
  let height = h - margin.top - margin.bottom

  g <- svg ... append "g"
    ..  attr "transform"  (SetAttr ("translate(32," <> show (height / 2.0) <> ")"))

  update alphabet g

  pure unit
