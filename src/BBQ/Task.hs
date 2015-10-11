module BBQ.Task where

import BBQ.Import
import qualified Data.HashMap.Lazy as HM
import BBQ.FTree

data RecursiveTask meta s = RecursiveTask {
    extension   :: FilePath,
    extract     :: FilePath -> Text -> Text -> Maybe meta,
    summarize   :: FilePath -> [FilePath] -> HashMap FilePath meta -> s,
    renderIndex :: s -> Action Text,
    renderPage  :: s -> meta -> Action Text -- Source path, rendered text
}

runRecTask :: FilePath -> RecursiveTask m s -> PathTree -> Rules ()
runRecTask buildPath rec@RecursiveTask{..} pathTree@(Dir _ _) = do
    let (Dir rootDir subTrees) = filterFTree (\f -> takeExtension f == "." ++ extension) pathTree
    let files = filterFiles subTrees
    let dirs  = filterDirs subTrees

    want [buildPath </> rootDir </> "index.html"]
    want $ map (\f -> buildPath </> f -<.> ".html") files

    getMS <- newCache' $ do
        need files
        texts <- mapM readFile' files
        gitDates <- mapM getGitDate files
        let metas = catMaybes $ map (\(f, d, t) -> extract f d t) (zip3 files gitDates texts)
        let filenames = map (dropExtension . takeFileName) files
        let metaMap = HM.fromList $ zip filenames metas
        let summary = summarize rootDir (map (\(Dir x _) -> x) dirs) metaMap
        return (metaMap, summary)


    buildPath </> rootDir </> "*.html" %> \out -> do
        (metaMap, summary) <- getMS
        case HM.lookup (dropExtension $ takeFileName out) metaMap of
            Nothing   -> return ()
            Just meta -> renderPage summary meta >>= writeFile' out

    buildPath </> rootDir </> "index.html" %> \out -> do
        (_, summary) <- getMS
        t <- renderIndex summary
        writeFile' out t

    forM_ dirs $ runRecTask buildPath rec
  where
    getGitDate p = do
        let gitCmd = "git log -1 --format=%ci --" :: String
        Stdout gitDate <- cmd gitCmd [unpack p]
        return (pack gitDate)


runRecTask _ _ _ = error "Impossible happens"

