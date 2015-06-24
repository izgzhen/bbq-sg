module BBQ.SG.Config where

data Config = Config_ {
    _staticDir   :: FilePath,
    _markdownDir :: FilePath,
    _imgSrcDir   :: FilePath,
    _postsDir    :: FilePath,
    _imgStaDir   :: FilePath,
    _analyticsId :: String
}