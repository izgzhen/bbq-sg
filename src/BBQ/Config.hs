module BBQ.Config where

import Language.Haskell.TH
import System.FilePath

templDirQ :: String -> Q Exp
templDirQ s = [| "templates" </> s |]


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
    mdSrcDir   = "markdowns",
    siteConfig = defSiteConfig
}

data SiteConfig = SiteConfig {
    host   :: String,
    author :: String
}

defSiteConfig = SiteConfig {
    host   = "http://localhost",
    author = "yourname"
}