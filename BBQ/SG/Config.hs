module BBQ.SG.Config where

-- The configuration which needs to be provided by user
data Config = Config_ {
    _staticDir   :: FilePath,
    _markdownDir :: FilePath,
    _imgSrcDir   :: FilePath,
    _postsDir    :: FilePath,
    _imgStaDir   :: FilePath,
    _jsSrcDir    :: FilePath,
    _jsStaDir    :: FilePath,
    _cssSrcDir   :: FilePath,
    _cssStaDir   :: FilePath,
    _analyticsId :: String
}

