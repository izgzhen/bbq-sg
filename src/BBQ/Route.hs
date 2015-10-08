module BBQ.Route where

import ClassyPrelude
import Web.Routes
import Web.Routes.TH
import BBQ.Config
import System.FilePath

newtype PostId = PostId { unPostId :: Text }
    deriving (PathInfo)

data WebPath = Home
             | Post PostId
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
