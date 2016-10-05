module Miserables where

type GroupedForceLayout = { nodes :: Array Node
                          , links :: Array Link }

type Node = { id :: String, group :: Number}
type Link = { source :: String, target :: String, value :: Number }

foreign import miserables :: GroupedForceLayout
