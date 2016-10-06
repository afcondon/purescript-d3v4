module Miserables (
    miserables
  , makeDraggable
  ) where

import Prelude (map)
import D3.ForceSimulation

foreign import miserables :: GroupedForceLayout

convert :: Node -> DraggableNode
convert { id: i, group: g } = { id: i, group: g, x: 0.0, y: 0.0 }

convertedNodes :: Array DraggableNode
convertedNodes = map convert miserables.nodes

makeDraggable :: GroupedForceLayout -> DraggableLayout
makeDraggable g = { nodes: (map convert g.nodes), links: g.links }
