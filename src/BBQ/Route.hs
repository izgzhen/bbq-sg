module BBQ.Route (
  absolutePath
, filePath
, absolutePathNoEscape
, PostId(..)
, WikiId(..)
, WebPath(..)
, msgTranlator
, contentRender
) where

import ClassyPrelude
import Web.Routes
import Web.Routes.TH
import BBQ.Config
import System.FilePath
import Text.Hamlet
import Text.Blaze.Html (toHtml)
import Data.Aeson (ToJSON, FromJSON)

newtype PostId = PostId { unPostId :: Text }
    deriving (PathInfo, Eq, Show, Ord, Generic)

instance ToJSON PostId
instance FromJSON PostId

newtype WikiId = WikiId { unWikiId :: Text }
    deriving (PathInfo, Eq, Show, Ord, Generic)

instance ToJSON WikiId
instance FromJSON WikiId

data WebPath = Index
             | Post PostId
             | Wiki WikiId
             deriving (Typeable)

$(derivePathInfo ''WebPath)

class ToPath x where
    toURLPath  :: x -> FilePath
    toFilePath :: x -> FilePath

instance ToPath WebPath where
    toURLPath p = unpack $ encodePathInfo (toPathSegments p) []
    toFilePath p = joinPath (map unpack (toPathSegments p))

instance ToPath FilePath where
    toURLPath p = unpack $ encodePathInfo [pack p] []
    toFilePath = id

absolutePath :: ToPath p => SiteConfig -> p -> FilePath
absolutePath sconf p = unpack (host sconf) ++ toURLPath p

absolutePathNoEscape :: ToPath p => SiteConfig -> p -> FilePath
absolutePathNoEscape sconf p = unpack (host sconf) ++ toFilePath p


filePath :: ToPath p => BuildConfig -> p -> FilePath
filePath BuildConfig{..} p = targetDir </> toFilePath p ++ (".html" :: String)

-- They are defined internally in Text.Hamlet
type Render url = url -> [(Text, Text)] -> Text
type Translate msg = msg -> Html

msgTranlator :: Translate Text
msgTranlator = toHtml

contentRender :: Render WebPath
contentRender p params = encodePathInfo (toPathSegments p) (map f params)
    where
        f :: (Text, Text) -> (Text, Maybe Text)
        f (a, "") = (a, Nothing)
        f (a, b)  = (a, Just b)



