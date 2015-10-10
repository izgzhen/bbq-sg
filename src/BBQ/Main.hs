module BBQ.Main where

import BBQ.Import
import BBQ.Task
import BBQ.Component.Post
import BBQ.Component.Index
import BBQ.Component.Wiki
import qualified Data.HashMap.Lazy as HM

hsDeps :: FilePath -> Action ()
hsDeps srcDir = do
    hs <- getDirectoryFiles "" [ srcDir </> "/*.hs" ]
    need hs

main :: IO ()
main =
    let config@BuildConfig{..} = defaultBuildConfig
    in shakeArgs shakeOptions { shakeFiles = targetDir } $ do
        let buildAt = (</>) targetDir
        phony "clean" $ removeFilesAfter targetDir ["//*"]
        buildAt "index.html" %> \out -> do
            hsDeps hsSrcDir
            mds <- getDirectoryFiles "" [ mdSrcDir </> "*.md" ]
            mPostWidget <- runTask mds postTask (\_ -> []) VoidWriteTask config
            -- Stage 1 ends
            runCollectTask config (HM.fromList $ catMaybes [mPostWidget]) indexStage2

        buildAt (wikiSrcDir </> "/*.html") %> \out -> do
            error_ $ pack $ "stab0: " ++ show out
            -- wikiMds <- getDirectoryFiles "" [ mdSrcDir </> "/*.md" ]
            -- need wikiMds

            -- runRecursiveTask config wikiTask wikiCollectTask wikiMkDirWidget "wiki"

        want [buildAt "index.html"]
        want [buildAt wikiSrcDir </> "2014-04-24-example.html"]



