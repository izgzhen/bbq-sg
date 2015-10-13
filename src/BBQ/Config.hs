module BBQ.Config (
  BuildConfig(..)
, defaultBuildConfig
, SiteConfig(..)
) where

import System.FilePath
import ClassyPrelude

-- Don't export the constructor, use record syntax instead
data BuildConfig = BuildConfig {
    targetDir  :: FilePath,
    hsSrcDir   :: FilePath,
    hsOther    :: [FilePath],
    mdSrcDir   :: FilePath,
    wikiSrcDir :: FilePath,
    siteConfig :: SiteConfig
}

defaultBuildConfig :: BuildConfig
defaultBuildConfig = BuildConfig {
    targetDir  = "build",
    hsSrcDir   = "src", -- XXX: overlapping with .cabal file
    hsOther    = [],
    mdSrcDir   = "post",
    wikiSrcDir = "wiki",
    siteConfig = defSiteConfig
}

data SiteConfig = SiteConfig {
    host   :: Text,
    author :: Text
}

defSiteConfig :: SiteConfig
defSiteConfig = SiteConfig {
    host   = "http://localhost:3000",
    author = "yourname"
}