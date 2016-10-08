module Main where

import D3.Selection
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import D3.Base (PolyValue(Value), AttrSetter(SetAttr, AttrFn), DataBind(Data), Point, D3, (..), (...))
import Prelude (pure, unit, Unit, bind)

circleData :: Array Point
circleData = [ {x: 100.0, y: 100.0}
             , {x: 200.0, y: 200.0}
             , {x: 100.0, y: 200.0}
             , {x: 200.0, y: 100.0}
             , {x: 150.0, y: 150.0}
             ]


main :: ∀ e. Eff (d3::D3,console::CONSOLE|e) Unit
main = do
  svg <- d3Select ".svg"
  g <-  svg ... append "g"

  circles <- g ... selectAll "circle"
      .. dataBind (Data circleData)
    .. enter .. append "circle"
      .. attr "cx" (AttrFn (\d i nodes el -> pure d.x)) -- thing to bear in mind here:
      .. attr "cy" (AttrFn (\d i nodes el -> pure d.y)) -- if you mod here doesn't change underlying value when you drag
      .. attr "r"  (SetAttr 20.0)
      .. style "stroke" (Value "black")
      .. style "fill"   (Value "red")

  pure unit
