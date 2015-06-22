module Config where
import System.FilePath ((</>))

rootDir     = "test"
staticDir   = rootDir   </> "static"
markdownDir = rootDir   </> "markdowns"
postsDir    = staticDir </> "posts"
imagesDir   = staticDir </> "images"
analyticsId = "UA-64349949-1"