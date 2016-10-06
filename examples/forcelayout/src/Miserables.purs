module Miserables (
    Node
  , Link
  , miserables
  , makeDraggable
  ) where

import D3.Base (Nodes)
import Prelude (map)

type Node = { id :: String, group :: Number }
type Link = { source :: String, target :: String, value :: Number }

type GroupedForceLayout = { nodes :: Array Node
                          , links :: Array Link }

-- for a type to be draggable it will have to have x and y fields for the drag callback to operate on
type DraggableNode = { id :: String, group :: Number, x :: Number, y :: Number }
type DraggableLayout = { nodes :: Array DraggableNode
                       , links :: Array Link }

foreign import miserables :: GroupedForceLayout

convert :: Node -> DraggableNode
convert { id: i, group: g } = { id: i, group: g, x: 0.0, y: 0.0 }

convertedNodes :: Array DraggableNode
convertedNodes = map convert miserables.nodes

makeDraggable :: GroupedForceLayout -> DraggableLayout
makeDraggable g = { nodes: (map convert g.nodes), links: g.links }
