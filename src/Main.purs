module Main where

import Control.Monad.Eff (Eff)
import Prelude (show, id, bind, (<>), (*), max)
import Data.Foldable (foldr)
import Graphics.D3.Base (D3)
import Graphics.D3.Util (maxFn, (..))
import Graphics.D3.Selection (Selection, text', style', append, enter, bindData, selectAll, rootSelect)
import Graphics.D3.Scale (toFunction, range, domain, linearScale)

array :: Array Number
array = [4.0, 8.0, 15.0, 16.0, 23.0, 42.0]

arrayMax = foldr max 0.0 array

main :: forall e. Eff ( d3 :: D3 | e ) (Selection Number)
main = do
  x <- linearScale
    .. domain [0.0, arrayMax]
    .. range [0.0, 420.0]
    .. toFunction

  rootSelect ".chart"
    .. selectAll "div"
      .. bindData array
    .. enter .. append "div"
      .. style' "width" (\d -> show (x d) <> "px")
      .. text' show
