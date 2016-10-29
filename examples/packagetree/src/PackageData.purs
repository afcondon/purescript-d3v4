module PackageData where

import Prelude
import D3.ForceSimulation (ForceType(Links), GroupedForceLayout)
import Data.Argonaut (gDecodeJson, Json, class DecodeJson, decodeJson, (.?))
import Data.Array (concatMap)
import Data.Generic (class Generic)
import Network.HTTP.Affjax (Affjax, get, URL)

--  read in the file
--  convert to desired format
--  make available to mainline for visualisation
--  stretch goal - load in various files and return array of data so that transforms can happen

-- | Ultra-simplistic types for now
type PackageName   = String
type VersionString = String

-- foreign import packagesJSON :: Json
-- foreign import packagesSmol :: Json
--
-- data PackageData = PackageData  { dependencies :: Array PackageName
--                                 , repo    :: URL
--                                 , version :: VersionString }
--
-- data PackageSet = StrMap PackageData
--
-- derive instance genericPackageData :: Generic PackageData
-- derive instance genericPackageSet  :: Generic PackageSet
--
-- instance decodeJsonPackageData :: DecodeJson PackageData where
--   decodeJson json = do
--     obj <- decodeJson json
--     dependencies <- obj .? "dependencies"
--     repo         <- obj .? "repo"
--     version      <- obj .? "version"
--     pure $ PackageData { dependencies, repo, version }
--
-- instance decodeJsonPackageSet :: DecodeJson PackageSet where
--   decodeJson json = do
--     obj <- gDecodeJson json
--     pure $ obj

-- Data defs for ForceLayout
type Node = { id :: String, group :: Number }
type Link = { source :: String, target :: String, value :: Number }

-- Data def (almost) for packages.json
type PackageSet = Array PackageData
type PackageData =  { name :: String
                    , data :: PackageDetails }

type PackageDetails = { dependencies :: Array PackageName
                      , repo         :: URL
                      , version      :: VersionString }

mydata = convert dummyData

-- | Conversion function
convert :: PackageSet -> GroupedForceLayout
convert ps = { nodes: map getNode ps
             , links: concatMap getLinks ps }
  where
    getLinks :: PackageData -> Array Link
    getLinks { name: name, data: { dependencies: deps } } = map (createLink name) deps
      where
        createLink s t = { source: s, target: t, value: 1.0 }
    getNode :: PackageData -> Node
    getNode { name: name } = { id: name, group: 0.0 }

dummyData :: PackageSet
dummyData = [ { name: "awn"
              , data: {
                  dependencies: []
                , repo: "some/path/"
                , version: "v1.0.0" } },
              { name: "bel"
              , data: {
                  dependencies: []
                , repo: "some/path/"
                , version: "v1.0.0" } },
              { name: "cep"
              , data: {
                  dependencies: []
                , repo: "some/path/"
                , version: "v1.0.0" } },
              { name: "dof"
              , data: {
                  dependencies: []
                , repo: "some/path/"
                , version: "v1.0.0" } },
              { name: "erg"
              , data: {
                  dependencies: []
                , repo: "some/path/"
                , version: "v1.0.0" } },
              { name: "fid"
              , data: {
                  dependencies: []
                , repo: "some/path/"
                , version: "v1.0.0" } },
              { name: "gid"
              , data: {
                  dependencies: []
                , repo: "some/path/"
                , version: "v1.0.0" } },
              { name: "hoy"
              , data: {
                  dependencies: []
                , repo: "some/path/"
                , version: "v1.0.0" } }
            ]
