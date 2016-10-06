module Miserables (
    miserables
  , makeDraggable
  ) where

import Prelude (map)
import D3.ForceSimulation

foreign import miserables :: GroupedForceLayout

-- this shouldn't be necessary because the extra fields should get added
-- transparently by the d3 simulation. the problem is, we have to know that the
-- row has been extended because callback functions will be dependent on those
-- values

-- ^^^^ this might not be true in which case this file is redundant and
-- miserables.js can become main.js

convert :: Node -> DraggableNode
convert { id: i, group: g } = { id: i, group: g, x: 0.0, y: 0.0 }

convertedNodes :: Array DraggableNode
convertedNodes = map convert miserables.nodes

makeDraggable :: GroupedForceLayout -> DraggableLayout
makeDraggable g = { nodes: (map convert g.nodes), links: g.links }
