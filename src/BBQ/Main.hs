module BBQ.Main where

import BBQ.Import
import BBQ.Task
import BBQ.Post
import BBQ.Index
import qualified Data.HashMap.Lazy as HM

main :: IO ()
main =
    let config@BuildConfig{..} = defaultBuildConfig
    in shakeArgs shakeOptions { shakeFiles = targetDir } $ do
        let buildAt = (</>) targetDir
        phony "clean" $ removeFilesAfter targetDir ["//*"]
        buildAt "index.html" %> \out -> do
            hs <- getDirectoryFiles "" [ hsSrcDir </> "/*.hs" ]
            need hs
            mPostWidget <- runTask out postTask config
            -- Stage 1 ends
            runCollectTask config (HM.fromList $ catMaybes [mPostWidget]) indexStage2

        want [buildAt "index.html"]



