module BBQ.Main where

import BBQ.Import
import BBQ.Config
import BBQ.Task
import BBQ.Post
import Development.Shake
import Development.Shake.FilePath

main :: IO ()
main =
    let config@BuildConfig{..} = defaultBuildConfig
        task = postTask
    in shakeArgs shakeOptions { shakeFiles = targetDir } $ do
        let buildAt = (</>) targetDir
        phony "clean" $ removeFilesAfter targetDir ["//*"]
        buildAt "markdowns/*.html" %> \out -> do
            hs   <- getDirectoryFiles "" [ hsSrcDir </> "/*.hs" ]
            tems <- getDirectoryFiles "" [ defaultTemplateDir </> "/*.hamlet"
                                         , defaultTemplateDir </> "/*.lucius"
                                         , defaultTemplateDir </> "/*.julius" ]
            need $ hs ++ tems
            runTask out task config
        -- buildAt "markdowns/*.html" %> \out -> do
        --     let src = dropExtension (dropDirectory1 out) ++ ".md"
        --     need [src]
        --     copyFile' src out
        want [buildAt "markdowns" </> "2014-04-24-example.html"]


