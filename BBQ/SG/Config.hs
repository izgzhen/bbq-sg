module BBQ.SG.Config where

-- The configuration which needs to be provided by user
data Config = Config_ {
    _srcDir      :: FilePath,
    _staticDir   :: FilePath,
    _markdownDir :: FilePath,
    _imgSrcDir   :: FilePath,
    _postsDir    :: FilePath,
    _imgStaDir   :: FilePath,
    _jsSrcDir    :: FilePath,
    _jsStaDir    :: FilePath,
    _cssSrcDir   :: FilePath,
    _cssStaDir   :: FilePath,
    _blacklist   :: FilePath,
    _analyticsId :: String
}

