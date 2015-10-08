module BBQ.Main where

import BBQ.Import
import BBQ.Task
import BBQ.Post

main :: IO ()
main =
    let config@BuildConfig{..} = defaultBuildConfig
        task = postTask
    in shakeArgs shakeOptions { shakeFiles = targetDir } $ do
        let buildAt = (</>) targetDir
        phony "clean" $ removeFilesAfter targetDir ["//*"]
        buildAt (mdSrcDir </> "*.html") %> \out -> do
            hs <- getDirectoryFiles "" [ hsSrcDir </> "/*.hs" ]
            need hs
            runTask out task config
        want [buildAt mdSrcDir </> "2014-04-24-example.html"]


