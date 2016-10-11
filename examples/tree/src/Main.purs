module Main where

import D3.Selection
import D3.Base
import D3.Tree
import Data.Maybe
import Prelude (show, pure, bind, (<>), Unit, unit, (-), ($), (/), (+), negate)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (drop)

type TreeName = String
type TreeIndex = String

type TreeNode = { id :: TreeName, index :: TreeIndex }

-- define a margin, look to purescript-css for more sophisticated definition
margin :: { top::Number, right::Number, bottom::Number, left::Number }
margin = { top: 20.0, right: 20.0, bottom: 30.0, left: 40.0}

foreign import flaredata :: Array TreeNode

linkPath :: ∀ d eff. HierarchyNode d -> Index -> Nodes -> D3Element -> Eff (d3::D3|eff) String
linkPath d i n e = do
    p <- parent d
    let pX = case p of
              (Just pd) -> pd.x
              Nothing   -> 0.0
    let pY = case p of
          (Just pd) -> pd.y
          Nothing  -> 0.0
    let dx = show pX
    let dy = show pY
    let x  = show d.x
    let y  = show d.y
    let midY = show ((d.y + pY) / 2.0)
    pure ("M" <> y <> "," <> x <> "C" <> midY <> "," <> x <> " " <> midY <> "," <> dx <> " " <> dy <> "," <> dx)

main :: ∀ e. Eff (d3::D3,console::CONSOLE|e) Unit
main = do
  svg <- d3Select ".svg"
  w   <- svg ... getAttr "width"
  h   <- svg ... getAttr "height"
  let width =  w - margin.left - margin.right
  let height = h - margin.top - margin.bottom

  g <-  svg ... append "g"

  h    <- d3Hierarchy
  tree <- d3Tree

  root   <- hierarchize flaredata h
  layout <- layoutTree root
  nodeDescendents <- descendents layout

  link <- g ... selectAll ".link"
           .. dataBind (Data (drop 1 nodeDescendents))
            .. enter .. append "path"
            .. attr "class"  (SetAttr "link")
            .. attr "d"      (AttrFn linkPath)

  node <- g ... selectAll ".node"
          .. dataBind (Data nodeDescendents)
            .. enter .. append "g"
            .. attr "class" (AttrFn (\d i n e -> "node"))
            .. attr "transform" (AttrFn (\d i n e -> "translate(" <> (show d.y) <> "," <> (show d.y) <> ")" ))

  node ... append "circle"
        .. attr "r" (SetAttr 2.5)

  node ... append "text"
        .. attr "dy" (SetAttr 3)
        .. attr "x"  (AttrFn (\d i n e -> if hasChildren d then -8.0 else 8.0 ))
        .. style "text-anchor" (SetSome (\d i n e -> if hasChildren d then "end" else "start"))
        .. text (SetByIndex (\d i -> d.data.name ))

  pure unit
