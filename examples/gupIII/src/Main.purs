module Main where

import D3.Selection
import Control.Monad.Eff.Console (CONSOLE)
import D3.Base (D3Element, Index, PolyValue(SetByIndex, Value), DataBind(Keyed), AttrSetter(AttrFn, SetAttr), D3, Eff, (...), (..), transparent, opaque)
import D3.Transitions (tRemove, AttrInterpolator(TweenFn, Target), tStyle, tAttr, savedTransition, TransitionName(..), duration, d3Transition)
import Data.String (singleton, toCharArray)
import Data.Traversable (traverse)
import Prelude (flip, map, Unit, unit, pure, bind, show, negate, (/), (<>), (-), (*))

makeTweenerFn :: ∀ d eff. d -> Index -> D3Element -> Eff (d3::D3|eff) (Number -> Number)
makeTweenerFn d i e = pure (\i -> i * 32.0)

-- define a margin, look to purescript-css for more sophisticated definition
margin :: { top::Number, right::Number, bottom::Number, left::Number }
margin = { top: 20.0, right: 20.0, bottom: 30.0, left: 40.0}

updateFn :: ∀ eff. Selection Char -> Array Char -> Eff (d3::D3|eff) Unit
updateFn g d = do
  -- | set up the transition that we'll use between phases
  t <- d3Transition (Name "t")
    .. duration 750.0

  -- || JOIN new data with old elements
  myText <- g ... selectAll "text"
    .. dataBind (Keyed d (\d -> d))

  -- || EXIT old elements not present in new data
  exit <- myText ... exit
        .. attr "class" (SetAttr "exit")
      .. savedTransition t
        .. tAttr "y"     (Target 60.0)
        .. tStyle "fill-opacity" (Target transparent)
        .. tRemove

  -- || UPDATE old elements present in new data
  update <- myText ... attr "class" (SetAttr "update")
      .. attr "y" (SetAttr 0.0)
      .. style "fill-opacity" (Value opaque)
    .. savedTransition t
      .. tAttr "x" (TweenFn makeTweenerFn) -- can't lambda function factory, i believe

  -- || ENTER new elements present in new data
  enter <- myText ... enter .. append "text"
      .. attr "class" (SetAttr "enter")
      .. attr "dy"    (SetAttr ".35em")
      .. attr "y"     (SetAttr (negate 60.0))
      .. attr "x"     (AttrFn (\d i n e -> pure (show (i * 32.0))))
      .. style "fill-opacity" (Value transparent)
      .. text (SetByIndex (\d i -> pure (singleton d)))

  enter  ... savedTransition t
      .. tAttr "y" (Target 0.0)
      .. tStyle "fill-opacity" (Target opaque)

  pure unit

alphabet :: Array Char
alphabet = toCharArray "abcdefghijklmnopqrstuvwxyz"

alphabets :: Array (Array Char)
alphabets = map toCharArray ["purescript", "by", "example"]

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

  updateFn g alphabet

  traverse (updateFn g) alphabets

  pure unit
