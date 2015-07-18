module BBQ.SG.Config where

import System.FilePath ((</>))

-- The configuration which needs to be provided by user

data Config = Config_ {
    _staticDir   :: FilePath,
    _srcDir      :: FilePath,

    _postsSrc    :: FilePath,
    _postsURL    :: FilePath,

    _imgSrc      :: FilePath,
    _imgURL      :: FilePath,

    _cssSrc      :: FilePath,
    _cssURL      :: FilePath,

    _jsSrc       :: FilePath,
    _jsURL       :: FilePath,

    _tagsURL     :: FilePath,

    _blacklist   :: FilePath,
    _analyticsId :: String
}

_postsSta config  = _staticDir config </> _postsURL config
_imgSta config    = _staticDir config </> _imgURL config
_cssSta config    = _staticDir config </> _cssURL config
_jsSta config     = _staticDir config </> _jsURL config
_tagsSta config   = _staticDir config </> _tagsURL config