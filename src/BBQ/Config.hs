module BBQ.Config (
  templDirQ
, BuildConfig(..)
, defaultBuildConfig
, SiteConfig(..)
) where

import Language.Haskell.TH
import System.FilePath
import Prelude (String)
import ClassyPrelude

defaultTemplateDir :: FilePath
defaultTemplateDir = "templates"

templDirQ :: FilePath -> Q Exp
templDirQ s = [| defaultTemplateDir </> s |]


-- Don't export the constructor, use record syntax instead
data BuildConfig = BuildConfig {
    targetDir  :: FilePath,
    hsSrcDir   :: FilePath,
    hsOther    :: [FilePath],
    mdSrcDir   :: FilePath,
    wikiSrcDir :: FilePath,
    siteConfig :: SiteConfig
}

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

defSiteConfig = SiteConfig {
    host   = "http://localhost",
    author = "yourname"
}