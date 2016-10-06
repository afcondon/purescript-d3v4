module D3.ForceSimulation where

import D3.Base (Index, D3, Eff)
import Data.Function.Eff (runEffFn1, EffFn2, runEffFn2, EffFn1)
import Data.Maybe (Maybe(Nothing, Just))


foreign import data D3Simulation :: *
foreign import data D3Force      :: *

type Node = { id :: String, group :: Number }
type Link = { source :: String, target :: String, value :: Number }

type GroupedForceLayout = { nodes :: Array Node
                          , links :: Array Link }

-- for a type to be draggable it will have to have x and y fields for the drag callback to operate on
type DraggableNode = { id :: String, group :: Number, x :: Number, y :: Number }
type DraggableLayout = { nodes :: Array DraggableNode
                       , links :: Array Link }

data ForceType      = Centering | Collision | Links | ManyBody | ForceX | ForceY
data SimulationType = Force

foreign import d3ForceSimulationFn :: ∀ eff. Eff (d3::D3|eff) D3Simulation

foreign import addLinkForceFn      :: ∀ eff. EffFn2 (d3::D3|eff) D3Force D3Simulation D3Simulation

foreign import makeLinkForceFnFn   :: ∀ v eff. EffFn2 (d3::D3|eff) (Array Link) (Node -> Index -> v) D3Force

foreign import makeLinkForceFn     :: ∀ v eff. EffFn1 (d3::D3|eff) (Array Link) D3Force

d3ForceSimulation :: ∀ eff. SimulationType -> Eff (d3::D3|eff) D3Simulation
d3ForceSimulation Force = d3ForceSimulationFn

addLinkForce :: ∀ eff. D3Force -> D3Simulation -> Eff (d3::D3|eff) D3Simulation
addLinkForce = runEffFn2 addLinkForceFn

makeLinkForce :: ∀ v eff. Array Link -> Maybe (Node -> Index -> v) -> Eff (d3::D3|eff) D3Force
makeLinkForce ls (Just f) = runEffFn2 makeLinkForceFnFn ls f
makeLinkForce ls Nothing  = runEffFn1 makeLinkForceFn   ls

-- || Minimal implementation for simple demo:
  -- var simulation = d3.forceSimulation()
  --     .force("link", d3.forceLink().id(function(d) { return d.id; }))
  --     .force("charge", d3.forceManyBody())
  --     .force("center", d3.forceCenter(width / 2, height / 2));

  -- simulation
  --     .nodes(graph.nodes)
  --     .on("tick", ticked);

  -- simulation.force("link")
  --     .links(graph.links);


{-
Simulation API
  on
  find
  stop
  tick
  alpha
  force
  nodes
  restart
  alphaMin
  alphaDecay
  alphaTarget
  velocityDecay

Simulations typically compose multiple forces as desired. This module provides several for your enjoyment:

                x y radius strength iterations links id distance theta distanceMin distanceMax
  Centering     * *
  Collision           *       *         *
  Links                       *         *        *     *     *
  Many-Body                   *                                     *       *          *
  PositioningX  *             *
  PositioningY    *           *

  Constructors for forces
    Centering   [x,y]
    Collision   [radius]
    Links       [Links]
    ManyBody
    ForceX      [x]
    ForceY      [y]
-}
