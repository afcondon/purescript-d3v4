module Main where

import Control.Monad.Eff.Console (CONSOLE)
import D3.Base (PolyValue(SetByIndex), D3, Eff, Index, D3Element, Nodes, AttrSetter(..), DataBind(..), (..), (...))
import D3.Selection (text, style, attr, append, enter, dataBind, selectAll, getAttr, d3Select)
import D3.Tree (parentsEq, separation, size, HierarchyNode, descendants, layoutTree, d3Hierarchy, d3Tree, hasChildren, parent)
import Data.Array (drop)
import Data.Maybe (Maybe(..))
import Math (sin, pi, cos)
import Prelude (show, pure, bind, (<>), Unit, unit, (-), ($), (/), (+), (*), (<), (<$>), negate)

type TreeName = String
type TreeIndex = String

type TreeNode = { name :: TreeName }

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
    let midY = (d.y + pY) / 2.0
    pure ("M" <> project d.x d.y <> "C" <> project d.x midY <> " " <> project pX midY <> " " <> project pX pY)

labelXOffset :: ∀ d eff. HierarchyNode d -> Index -> Nodes -> D3Element -> Eff (d3::D3|eff) Number
labelXOffset d i n e = do
  cs <- hasChildren d
  pure (if cs then -8.0 else 8.0)

labelAnchor :: ∀ d eff. HierarchyNode d -> Index -> Eff (d3::D3|eff) String
labelAnchor d i = do
  cs <- hasChildren d
  pure (if cs then "start" else "end")

labelRotate :: ∀ d eff. HierarchyNode d -> Index -> Nodes -> D3Element -> Eff (d3::D3|eff) String
labelRotate d i n e = pure ("rotate(" <> show angle <> ")")
  where angle = if d.x < 180.0 then d.x - 90.0 else d.x + 90.0

project :: Number -> Number -> String
project x y = show px <> "," <> show py
  where
    angle  = (x - 90.0) / 180.0 * pi
    radius = y
    px     = radius * (cos angle)
    py     = radius * (sin angle)

centerOffset :: Number -> Number -> Number -> Number -> String
centerOffset w h x y = show dx <> "," <> show dy
  where
    dx = w / 2.0 + x
    dy = h / 2.0 + y

main :: ∀ e. Eff (d3::D3,console::CONSOLE|e) Unit
main = do
  let treedata = flaredata

  svg <- d3Select ".svg"
  w   <- svg ... getAttr "width"
  h   <- svg ... getAttr "height"
  let width =  w - margin.left - margin.right
  let height = h - margin.top - margin.bottom

  g      <- svg ... append "g"
                ..  attr "transform" (AttrFn (\d i n e -> pure ("translate(" <> centerOffset width height 40.0 90.0 <> ")" )))
  tree   <- d3Tree
          .. size 360.0 500.0
          .. separation (\a b -> pure if parentsEq a b then 1.0 / a.depth else 2.0 / a.depth)

  root   <- d3Hierarchy treedata
  layout <- layoutTree root tree
  nodeDescendants <- descendants layout

  link <- g ... selectAll ".link"
           .. dataBind (Data (drop 1 nodeDescendants))
            .. enter .. append "path"
            .. attr "class"  (SetAttr "link")
            .. attr "d"      (AttrFn linkPath)
  --
  node <- g ... selectAll ".node"
          .. dataBind (Data nodeDescendants)
            .. enter .. append "g"
            .. attr "class" (AttrFn (\d i n e -> pure "node"))
            -- .. attr "transform" (AttrFn (\d i n e -> pure ("translate(" <> (show d.y) <> "," <> (show d.x) <> ")") ))
            .. attr "transform" (AttrFn (\d i n e -> pure ("translate(" <> project d.x d.y <> ")") ))

  node ... append "circle"
        .. attr "r" (SetAttr 2.5)

  node ... append "text"
        .. attr "dy" (SetAttr 3)
        .. attr "x"  (AttrFn labelXOffset)
        .. style "text-anchor" (SetByIndex labelAnchor)
        .. attr "transform" (AttrFn labelRotate)
        .. text (SetByIndex (\d i -> pure d.data.name ))

  pure unit
