module PackageAjax where

import Prelude
import PackageData
import Network.HTTP.Affjax (URL)

file :: URL
file = "https://raw.githubusercontent.com/purescript/package-sets/psc-0.10.1/packages.json"

-- type Affjax e a = Aff (ajax :: AJAX | e) (AffjaxResponse a)
-- type AffjaxResponse a = { status :: StatusCode, headers :: Array ResponseHeader, response :: a }
-- get :: forall e a. Respondable a => URL -> Affjax e a

-- readPackageData :: ∀ e a. URL -> Affjax e a
-- readPackageData url =
--   get url
--
--
-- main :: forall e. Eff (err :: EXCEPTION, ajax :: AJAX, console :: CONSOLE | e) Unit
-- main = void $ launchAff $ do
--   res <- post "http://jsonplaceholder.typicode.com/posts" (encodeJson post1)
--   liftEff $ log $ "response: " <> res.response
--
--
-- class Respondable a where
-- Members
--
-- fromResponse :: ResponseContent -> F a
-- responseType :: Tuple (Maybe MediaType) (ResponseType a)
-- Instances
--
-- Respondable Json
-- Respondable ArrayBuffer
-- Respondable Unit
-- Respondable String
-- Respondable Foreign
-- Respondable Document
-- Respondable Blob
-- Source
