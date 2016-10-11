module Main where

import D3.Selection
import Control.Monad.Eff.Console (CONSOLE, log)
import D3.Base
import Data.Maybe
import Prelude

type TreeName = String
type TreeIndex = String

type TreeNode = { id :: TreeName, index :: TreeIndex }

foreign import data flaredata = Array TreeNode

main :: ∀ e. Eff (d3::D3,console::CONSOLE|e) Unit
main = do
  pure unit
