module Config where
import System.FilePath ((</>))

rootDir     = "test"
staticDir   = rootDir   </> "static"
srcDir      = rootDir   </> "src"
markdownDir = srcDir    </> "markdowns"
imgSrcDir   = srcDir    </> "images"
postsDir    = staticDir </> "posts"
imgStaDir   = staticDir </> "images"
analyticsId = "UA-64349949-1"