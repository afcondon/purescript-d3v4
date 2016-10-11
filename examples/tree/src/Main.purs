module Main where

import D3.Selection (attr, append, enter, dataBind, selectAll, getAttr, d3Select)
import D3.Base (D3, Eff, Index, D3Element, Nodes, AttrSetter(..), DataBind(..), (..), (...))
import D3.Tree (HierarchyNode, descendants, layoutTree, hierarchize, d3Tree, d3Hierarchy, hasChildren, parent)
import Data.Maybe (Maybe(..))
import Prelude (show, pure, bind, (<>), Unit, unit, (-), ($), (/), (+), (<$>),negate)
import Control.Monad.Eff.Console (CONSOLE)
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

labelXOffset :: ∀ d eff. HierarchyNode d -> Index -> Nodes -> D3Element -> Eff (d3::D3|eff) Number
labelXOffset d i n e = do
  cs <- hasChildren d
  pure (if cs then -8.0 else 8.0)

labelAnchor :: ∀ d eff. HierarchyNode d -> Index -> Eff (d3::D3|eff) String
labelAnchor d i = do
  cs <- hasChildren d
  pure (if cs then "end" else "start")

main :: ∀ e. Eff (d3::D3,console::CONSOLE|e) Unit
main = do
  let treedata = flaredata

  svg <- d3Select ".svg"
  w   <- svg ... getAttr "width"
  h   <- svg ... getAttr "height"
  let width =  w - margin.left - margin.right
  let height = h - margin.top - margin.bottom

  g      <- svg ... append "g"
  tree   <- d3Tree

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
            .. attr "transform" (AttrFn (\d i n e -> pure ("translate(" <> (show d.y) <> "," <> (show d.y) <> ")") ))

  node ... append "circle"
        .. attr "r" (SetAttr 2.5)

  -- node ... append "text"
  --       .. attr "dy" (SetAttr 3)
  --       .. attr "x"  (AttrFn labelXOffset)
  --       .. style "text-anchor" (SetByIndex labelAnchor)
  --       .. text (SetByIndex (\d i -> d.data.name ))

  pure unit
