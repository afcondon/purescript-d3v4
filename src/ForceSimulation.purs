module D3.ForceSimulation where

import Data.Pair
import D3.Base (Index, D3, Eff)
import D3.Selection (Selection)
import Data.Function.Eff (EffFn2, EffFn1, EffFn3, runEffFn1, runEffFn2, runEffFn3)
import Data.Function.Uncurried (mkFn2, Fn2)
import Data.Maybe (Maybe(Nothing, Just))
import Prelude (Unit)


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
foreign import linkIDFn            :: ∀ v eff. EffFn2 (d3::D3|eff) (Fn2 Node Index v) D3Force      D3Force
foreign import makeCenterForceFn   :: ∀ eff. Eff    (d3::D3|eff)                                     D3Force
foreign import makeCenterForceFnP  :: ∀ eff. EffFn2 (d3::D3|eff) Number Number                       D3Force
foreign import makeLinkForceFn     :: ∀ eff. EffFn1 (d3::D3|eff) (Array Link)                        D3Force
foreign import makeManyBodyForceFn :: ∀ eff. Eff    (d3::D3|eff)                                     D3Force
foreign import getLinksFn          :: ∀ eff. EffFn1 (d3::D3|eff) D3Simulation                   (Array Link)
foreign import setLinksFn          :: ∀ eff. EffFn2 (d3::D3|eff) (Array Link) D3Force                D3Force
foreign import getForceFn          :: ∀ eff. EffFn2 (d3::D3|eff) String       D3Simulation           D3Force
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

addForce :: ∀ eff. ForceType -> String -> D3Force -> D3Simulation -> Eff (d3::D3|eff) D3Simulation
addForce Centering = runEffFn3 addForceFn
addForce Collision = runEffFn3 addForceFn -- "not implemented yet"
addForce Links     = runEffFn3 addForceFn
addForce ManyBody  = runEffFn3 addForceFn
addForce ForceX    = runEffFn3 addForceFn -- "not implemented yet"
addForce ForceY    = runEffFn3 addForceFn -- "not implemented yet"

-- This function will blow up if String doens't look up a valid force on this simulation TODO
getForce :: ∀ eff. String -> D3Simulation                         -> Eff (d3::D3|eff) D3Force
getForce name sim = runEffFn2 getForceFn name sim

-- || functions only for LINK force
makeLinkForce :: ∀ eff. Maybe (Array Link) -> Eff (d3::D3|eff) D3Force
makeLinkForce (Just ls) = runEffFn1 makeLinkForceFn   ls
makeLinkForce Nothing   = runEffFn1 makeLinkForceFn   []

setIDFunction :: ∀ v eff. (Node -> Index -> v) -> D3Force -> Eff (d3::D3|eff) D3Force -- Force HAS TO BE LINK FORCE HERE
setIDFunction f = runEffFn2 linkIDFn (mkFn2 f)

setLinks      :: ∀ eff. (Array Link) -> D3Force -> Eff (d3::D3|eff) D3Force
setLinks      = runEffFn2 setLinksFn

getLinks      :: ∀ eff. D3Simulation            -> Eff (d3::D3|eff) (Array Link)
getLinks      = runEffFn1 getLinksFn

-- || functions only for MANY BODY force
makeManyBody :: ∀ eff. Eff (d3::D3|eff) D3Force
makeManyBody = makeManyBodyForceFn

-- || functions only for CENTERING force
makeCenterForce :: ∀ eff. Maybe (Pair Number) -> Eff (d3::D3|eff) D3Force
makeCenterForce (Just (Pair x y)) = runEffFn2 makeCenterForceFnP x y
makeCenterForce Nothing           = makeCenterForceFn

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
