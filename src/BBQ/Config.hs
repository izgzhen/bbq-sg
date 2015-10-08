module BBQ.Config where

import Language.Haskell.TH
import System.FilePath
import Prelude (String)
import ClassyPrelude

defaultTemplateDir :: String
defaultTemplateDir = "templates"

templDirQ :: String -> Q Exp
templDirQ s = [| defaultTemplateDir </> s |]


-- Don't export the constructor, use record syntax instead
data BuildConfig = BuildConfig {
    targetDir  :: String,
    hsSrcDir   :: String,
    hsOther    :: [String],
    mdSrcDir   :: String,
    siteConfig :: SiteConfig
}

defaultBuildConfig = BuildConfig {
    targetDir  = "build",
    hsSrcDir   = "src", -- XXX: overlapping with .cabal file
    hsOther    = [],
    mdSrcDir   = "post",
    siteConfig = defSiteConfig
}

data SiteConfig = SiteConfig {
    host   :: Text,
    author :: Text
}

defSiteConfig = SiteConfig {
    host   = "http://localhost",
    author = "yourname"
}