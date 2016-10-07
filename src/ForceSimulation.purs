module D3.ForceSimulation where

import Data.Pair
import D3.Base (Index, D3, Eff)
import D3.Selection (Selection)
import Data.Function.Eff (mkEffFn1, mkEffFn2, runEffFn3, EffFn3, runEffFn1, EffFn2, runEffFn2, EffFn1)
import Data.Maybe (Maybe(Nothing, Just))
import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)

foreign import data D3Simulation :: *
foreign import data D3Force      :: *

type Node = { id :: String, group :: Number }
type Link = { source :: String, target :: String, value :: Number }

type GroupedForceLayout = { nodes :: Array Node
                          , links :: Array Link }

-- for a type to be draggable it will have to have x and y fields for the drag callback to operate on
type ForceNode = { id :: String, group :: Number, x :: Number, y :: Number }
type ForceLink = { source :: ForceNode, target :: ForceNode, value :: Number }
type DraggableLayout = { nodes :: Array ForceNode
                       , links :: Array Link }

data ForceType      = Centering | Collision | Links | ManyBody | ForceX | ForceY
data SimulationType = Force

foreign import addForceFn          :: ∀ eff. EffFn3 (d3::D3|eff) String D3Force D3Simulation    D3Simulation
foreign import d3ForceSimulationFn :: ∀ eff. Eff    (d3::D3|eff)                                D3Simulation
foreign import makeCenterForceFn   :: ∀ eff. Eff    (d3::D3|eff)                                     D3Force
foreign import makeCenterForceFnP  :: ∀ eff. EffFn1 (d3::D3|eff) (Array Number)                      D3Force
foreign import makeLinkForceFn     :: ∀ eff. EffFn1 (d3::D3|eff) (Array Link)                        D3Force
foreign import makeLinkForceFnFn :: ∀ v eff. EffFn2 (d3::D3|eff) (Array Link) (Node -> Index -> v)   D3Force
foreign import makeManyBodyForceFn :: ∀ eff. Eff    (d3::D3|eff)                                     D3Force
foreign import simulationNodesFn   :: ∀ eff. EffFn2 (d3::D3|eff) (Array Node) D3Simulation      D3Simulation
foreign import onTickFn            :: ∀ eff. EffFn2 (d3::D3|eff)
                                                    (Eff (d3::D3|eff) Unit)
                                                    D3Simulation
                                                    D3Simulation
foreign import defaultTickFn       :: ∀ eff. EffFn2 (d3::D3|eff) (Selection Node) (Selection Link)      Unit

defaultTick :: ∀ eff. Selection Node -> Selection Link -> Eff (d3::D3|eff) Unit
defaultTick = runEffFn2 defaultTickFn

d3ForceSimulation :: ∀ eff. SimulationType -> Eff (d3::D3|eff) D3Simulation
d3ForceSimulation Force = d3ForceSimulationFn

initNodes :: ∀ eff. Array Node -> D3Simulation -> Eff (d3::D3|eff) D3Simulation
initNodes = runEffFn2 simulationNodesFn

onTick  :: forall eff. Eff (d3::D3|eff) Unit -> D3Simulation -> Eff (d3::D3|eff) D3Simulation
onTick = runEffFn2 onTickFn

controlSelectionN :: ∀ eff. Selection Node -> Eff (d3::D3|eff) (Selection ForceNode)
controlSelectionN selection = unsafeCoerce selection

controlSelectionL :: ∀ eff. Selection Link -> Eff (d3::D3|eff) (Selection ForceLink)
controlSelectionL selection = unsafeCoerce selection

addForce :: ∀ eff. ForceType -> D3Force -> D3Simulation -> Eff (d3::D3|eff) D3Simulation
addForce Centering = runEffFn3 addForceFn "center"
addForce Collision = runEffFn3 addForceFn "not implemented yet"
addForce Links     = runEffFn3 addForceFn "link"
addForce ManyBody  = runEffFn3 addForceFn "charge"
addForce ForceX    = runEffFn3 addForceFn "not implemented yet"
addForce ForceY    = runEffFn3 addForceFn "not implemented yet"

makeLinkForce :: ∀ v eff. Maybe (Array Link) -> Maybe (Node -> Index -> v) -> Eff (d3::D3|eff) D3Force
makeLinkForce (Just ls) (Just f) = runEffFn2 makeLinkForceFnFn ls f
makeLinkForce (Just ls) Nothing  = runEffFn1 makeLinkForceFn   ls
makeLinkForce _ _                = runEffFn1 makeLinkForceFn   []

makeManyBody :: ∀ eff. Eff (d3::D3|eff) D3Force
makeManyBody = makeManyBodyForceFn

makeCenterForce :: ∀ eff. Maybe (Pair Number) -> Eff (d3::D3|eff) D3Force
makeCenterForce (Just (Pair x y)) = runEffFn1 makeCenterForceFnP [x,y]
makeCenterForce Nothing           = makeCenterForceFn


 -- var simulation = d3.forceSimulation()
 --                    .force("link", d3.forceLink().id(function(d) { return d.id; }))
 --                    .force("charge", d3.forceManyBody())
 --                    .force("center", d3.forceCenter(width / 2, height / 2));

  -- simulation
  --     .nodes(graph.nodes)
  --     .on("tick", ticked);

  -- simulation.force("link")
  --     .links(graph.links);

  -- function ticked() {
  --   link
  --       .attr("x1", function(d) { return d.source.x; })
  --       .attr("y1", function(d) { return d.source.y; })
  --       .attr("x2", function(d) { return d.target.x; })
  --       .attr("y2", function(d) { return d.target.y; });
  --
  --   node
  --       .attr("cx", function(d) { return d.x; })
  --       .attr("cy", function(d) { return d.y; });
  -- }

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
