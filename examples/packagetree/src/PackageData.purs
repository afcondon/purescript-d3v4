module PackageData where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import D3.Base (D3)
import D3.ForceSimulation (GroupedForceLayout)
import Data.Array (zip, concatMap, (!!), (:), (..), nub, length)
import Data.Either (Either(Right, Left))
import Data.Foldable (foldr)
import Data.Foreign (Foreign, F, ForeignError)
import Data.Foreign.Class (readJSON, readProp, class IsForeign)
import Data.Foreign.Index (prop)
import Data.Foreign.Keys (keys)
import Data.Int (toNumber)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (fromMaybe)
import Data.String (joinWith, split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (URL, AffjaxResponse)

defaultPackages :: URL
defaultPackages = "https://raw.githubusercontent.com/purescript/package-sets/psc-0.10.1/packages.json"

-- | Ultra-simplistic types for now
type PackageName   = String
type VersionString = String

-- Data defs for ForceLayout
type GroupID = Number
type Node = { id :: String, group :: GroupID }
type Link = { source :: String, target :: String, value :: Number }

-- Data def for packages.json
newtype PackageSet  = PackageSet (Array PackageData)
newtype PackageData = PackageData { name :: String
                                  , dependencies :: Array PackageName
                                  , repo         :: URL
                                  , version      :: VersionString }

instance showPackageData :: Show PackageData where
  show (PackageData { name, dependencies, repo, version }) = "name: " <> name <> "\n"
                                                           <> "dependencies: " <> joinWith ", " dependencies <> "\n"
                                                           <> "repo: " <> repo <> "\n"
                                                           <> "version: " <> version

instance showPackageSet :: Show PackageSet where
  show (PackageSet ps) = joinWith "\n" (map show ps)

readPackageData :: String -> Foreign -> F PackageData
readPackageData name f = do
  dependencies <- readProp "dependencies" f
  repo         <- readProp "repo" f
  version      <- readProp "version" f
  pure (PackageData { name, dependencies, repo, version })

instance isForeignPackageSet :: IsForeign PackageSet where
  read f = do
    packageNames <- keys f
    packageData  <- traverse (\name -> prop name f >>= readPackageData name) packageNames
    pure (PackageSet packageData)


myError :: forall e. Error -> Eff e Unit
myError e = pure unit

getGroupedLayoutData :: String -> Either ForeignError PackageSet
getGroupedLayoutData s = readJSON s  -- read the String as a PackageSet and discard error into Nothing if it doesn't go

mySuccess :: forall e. (GroupedForceLayout -> Eff (d3::D3|e) Unit) -> AffjaxResponse String -> Eff (d3::D3|e) Unit
mySuccess d3Render res = do
  let graph = case getGroupedLayoutData res.response of
              Left err -> mydata  -- default if file fetch failed or convert failed or whatever
              Right ps  -> convert ps (getGroups ps)
  d3Render graph
  pure unit


-- | convert to a format suitable for D3 Force Layout
convert :: PackageSet -> Map RepoName Number -> GroupedForceLayout
convert (PackageSet ps) groups = { nodes: makeNodes
                                 , links: concatMap getLinks ps }
  where
    namesList = nub $ (map getName ps) <> (concatMap getDepNames ps)
    -- need a map of the packages for which we have group info, ie the ones
    -- explicitly listed and not just found thru dependency lists
    makeNodes :: Array Node
    makeNodes = map makeNode namesList
    makeNode:: String -> Node
    makeNode name = { id: name, group: groupID }
      where
        groupID = fromMaybe 0.0 (lookup name mapNamesToGroups)
    mapNamesToGroups :: Map String GroupID
    mapNamesToGroups = fromFoldable $ foldr (\pd acc -> (getGroup pd):acc) [] ps
    getName :: PackageData -> String
    getName (PackageData { name }) = name
    getDepNames :: PackageData -> Array String
    getDepNames (PackageData { dependencies: deps }) = deps
    getLinks :: PackageData -> Array Link
    getLinks (PackageData { name, dependencies }) = map (createLink name) dependencies
      where
        createLink s t = { source: s, target: t, value: 1.0 }
    getGroup :: PackageData -> Tuple String GroupID
    getGroup (PackageData {name, repo}) = Tuple name groupID where
      key   = getRepoName repo
      groupID = fromMaybe 0.0 (lookup key groups)

-- Utilities for conversion
type RepoName = String

getRepoName :: URL -> RepoName   -- drops the "http:" "" "github.com" parts and the sub-dirs
getRepoName repo = fromMaybe "parse error" (subs !! 3)
  where
    subs = split "/" repo


getGroups :: PackageSet -> Map RepoName Number
getGroups (PackageSet ps) = fromFoldable $ zip uniqueRepos indices
  where
    uniqueRepos = nub $ foldr (\(PackageData p) acc -> (getRepoName p.repo):acc ) [] ps
    indices     = map toNumber (1..length uniqueRepos)


-- dummy data used in initial testing
mydata :: GroupedForceLayout
mydata = convert dummyData (getGroups dummyData)

dummyData :: PackageSet
dummyData = PackageSet [
  PackageData {
    name: "aff",
    "dependencies": [
      "arrays",
      "bifunctors",
      "console",
      "const",
      "contravariant",
      "control",
      "distributive",
      "eff",
      "either",
      "exceptions",
      "foldable-traversable",
      "functions",
      "functors",
      "identity",
      "invariant",
      "lazy",
      "maybe",
      "monoid",
      "newtype",
      "nonempty",
      "parallel",
      "partial",
      "prelude",
      "refs",
      "st",
      "tailrec",
      "transformers",
      "tuples",
      "unfoldable",
      "unsafe-coerce"
    ],
    "repo": "https://github.com/slamdata/purescript-aff.git",
    "version": "v2.0.0"
  },
  PackageData {
    name: "argonaut",
    "dependencies": [
      "argonaut-codecs",
      "argonaut-core",
      "argonaut-traversals",
      "arrays",
      "bifunctors",
      "const",
      "contravariant",
      "control",
      "distributive",
      "eff",
      "either",
      "enums",
      "foldable-traversable",
      "functions",
      "functors",
      "generics",
      "identity",
      "integers",
      "invariant",
      "lazy",
      "lists",
      "maps",
      "math",
      "maybe",
      "monoid",
      "newtype",
      "nonempty",
      "partial",
      "prelude",
      "profunctor",
      "profunctor-lenses",
      "proxy",
      "sets",
      "st",
      "strings",
      "tailrec",
      "transformers",
      "tuples",
      "unfoldable",
      "unsafe-coerce"
    ],
    "repo": "https://github.com/purescript-contrib/purescript-argonaut.git",
    "version": "v2.0.0"
  }
]
