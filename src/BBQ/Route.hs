module BBQ.Route (
  absolutePath
, filePath
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
    deriving (PathInfo)

data WebPath = Index
             | Post PostId
             | Wiki WikiId
             deriving (Typeable)

$(derivePathInfo ''WebPath)


absolutePath :: Monad m => WebPath -> ReaderT BuildConfig m FilePath
absolutePath p = do
    sconf <- siteConfig <$> ask
    let h = host sconf
    return $ unpack $ h ++ encodePathInfo (toPathSegments p) [] ++ ".html"

filePath :: Monad m => WebPath -> ReaderT BuildConfig m FilePath
filePath p = do
    tDir <- targetDir <$> ask
    return $ tDir </> joinPath (map unpack (toPathSegments p)) ++ (".html" :: String)

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



