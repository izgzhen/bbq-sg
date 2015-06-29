module Config where

import System.FilePath ((</>))

srcDir      = "."
staticDir   = "./static"
markdownDir = srcDir    </> "markdowns"
imgSrcDir   = srcDir    </> "images"
postsDir    = staticDir </> "posts"
imgStaDir   = staticDir </> "images"
jsSrcDir    = srcDir    </> "js"
cssSrcDir   = srcDir    </> "css"
jsStaDir    = staticDir </> "js"
cssStaDir   = staticDir </> "css"
analyticsId = "UA-64349949-1"
