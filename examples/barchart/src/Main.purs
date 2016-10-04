module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import D3.Base (PolyValue(Value), AttrSetter(SetAttr, AttrFn), DataBind(Data), D3, (..), (...))
import D3.Collections (D3Collection(D3Range, D3ArrT))

import D3.Scale (ScaleType(Linear, Band), scaleBy, bandwidth, domain, rangeRound, d3Scale, padding)
import D3.Selection (attr, append, enter, dataBind, selectAll, text, getAttr, d3Select)
import Prelude (map, Unit, unit, pure, bind, show, (-), (<>), ($), (>>=), (=<<))

type Pair = { letter :: Char, frequency :: Number }
frequencies :: Array Pair
frequencies = [
    { letter: 'A',	frequency: 0.08167 }
  , { letter: 'B',	frequency: 0.01492 }
  , { letter: 'C',	frequency: 0.02782 }
  , { letter: 'D',	frequency: 0.04253 }
  , { letter: 'E',	frequency: 0.12702 }
  , { letter: 'F',	frequency: 0.02288 }
  , { letter: 'G',	frequency: 0.02015 }
  , { letter: 'H',	frequency: 0.06094 }
  , { letter: 'I',	frequency: 0.06966 }
  , { letter: 'J',	frequency: 0.00153 }
  , { letter: 'K',	frequency: 0.00772 }
  , { letter: 'L',	frequency: 0.04025 }
  , { letter: 'M',	frequency: 0.02406 }
  , { letter: 'N',	frequency: 0.06749 }
  , { letter: 'O',	frequency: 0.07507 }
  , { letter: 'P',	frequency: 0.01929 }
  , { letter: 'Q',	frequency: 0.00095 }
  , { letter: 'R',	frequency: 0.05987 }
  , { letter: 'S',	frequency: 0.06327 }
  , { letter: 'T',	frequency: 0.09056 }
  , { letter: 'U',	frequency: 0.02758 }
  , { letter: 'V',	frequency: 0.00978 }
  , { letter: 'W',	frequency: 0.02360 }
  , { letter: 'X',	frequency: 0.00150 }
  , { letter: 'Y',	frequency: 0.01974 }
  , { letter: 'Z',	frequency: 0.00074 }
]

-- define a margin, look to purescript-css for more sophisticated definition
margin :: { top::Number, right::Number, bottom::Number, left::Number }
margin = { top: 20.0, right: 20.0, bottom: 30.0, left: 40.0}

main :: ∀ e. Eff (d3::D3,console::CONSOLE|e) Unit
main = do
  svg <- d3Select ".svg"

  w <- svg ... getAttr "width"
  h <- svg ... getAttr "height"
  let width =  w - margin.left - margin.right
  let height = h - margin.top - margin.bottom

  x <- d3Scale Band
        .. rangeRound 0.0 width
        .. padding 0.1
        .. domain (D3ArrT (map (\d -> d.letter) frequencies))

  y <- d3Scale Linear
        .. rangeRound height 0.0
        .. domain (D3Range 0.0 0.12702) -- implement max lookup later TODO

  g <-  svg ... append "g"
    ..  attr "transform"  (SetAttr ("translate(" <> show margin.left <> "," <> show margin.top <> ")"))

  g ... append "g"
    ..  attr "class"      (SetAttr "axis axis--x")
    ..  attr "transform"  (SetAttr ("translate(0," <> show height <> ")"))
    -- ..  call d3AxisBottom x

  g ... append "g"
    ..  attr "class"      (SetAttr "axis axis--y")
    -- ..  call d3AxisLeft y .. ticks 10 '%'
    .. append "text"
    .. attr "transform"   (SetAttr "rotate(-90)")
    .. attr "y"           (SetAttr 6.0)
    .. attr "dy"          (SetAttr "0.71em")
    .. attr "text-anchor" (SetAttr "end")
    .. text (Value "Frequency")

  g  ... selectAll (".bar")
      .. dataBind (Data frequencies)
    .. enter .. append "rect"
      .. attr "class"  (SetAttr "bar")
      .. attr "x"      (AttrFn (\d i nodes el -> scaleBy x d.letter))
      .. attr "y"      (AttrFn (\d i nodes el -> scaleBy y d.frequency))
      .. attr "width"  (SetAttr (bandwidth x))
      .. attr "height" (AttrFn (\d i nodes el -> do scaled <- scaleBy y d.frequency
                                                    pure (height - scaled)))
  pure unit
