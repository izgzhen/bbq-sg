module BBQ.SG.Config where
import BBQ.SG.Plugin
import System.FilePath ((</>))

-- The configuration which needs to be provided by user

data Config = Config_ {
    _staticDir   :: FilePath,
    _srcDir      :: FilePath,

    _postsSrc    :: FilePath,
    _postsURL    :: FilePath,

    _pageSrc     :: FilePath,
    _pageURL     :: FilePath,

    _wikiSrc     :: FilePath,
    _wikiURL     :: FilePath,

    _imgSrc      :: FilePath,
    _imgURL      :: FilePath,

    _cssSrc      :: FilePath,
    _cssURL      :: FilePath,
    _cssAbsURL   :: FilePath,

    _jsSrc       :: FilePath,
    _jsURL       :: FilePath,
    _jsAbsURL    :: FilePath,

    _tagsURL     :: FilePath,

    _blacklist   :: FilePath,
    _modCache    :: FilePath,
    _analyticsId :: String
}

_postsSta config  = _staticDir config </> _postsURL config
_pageSta config   = _staticDir config </> _pageURL config
_imgSta config    = _staticDir config </> _imgURL config
_cssSta config    = _staticDir config </> _cssURL config
_jsSta config     = _staticDir config </> _jsURL config
_tagsSta config   = _staticDir config </> _tagsURL config


resourceToHeader config (InternalJs p)  = scriptify (_jsAbsURL  config  </> p)
resourceToHeader config (InternalCss p) = cssify (_cssAbsURL config  </> p)
resourceToHeader config (ExternalJs p)  = scriptify p
resourceToHeader config (ExternalCss p) = cssify p

data ResourceSpec = InternalJs  String
                  | InternalCss String
                  | ExternalJs  String
                  | ExternalCss String
