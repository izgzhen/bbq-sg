module BBQ.Task where

import BBQ.Import
import qualified Data.HashMap.Lazy as HM
import BBQ.FTree

data Task meta s = Task {
    extension   :: FilePath,
    extract     :: FilePath -> Text -> Text -> Maybe meta,
    summarize   :: FilePath -> [FilePath] -> HashMap FilePath meta -> s, -- task implementor can choose to ignore the second parameter
    renderIndex :: s -> Action Text,
    renderPage  :: s -> meta -> Action Text -- Source path, rendered text
}

runRecTask :: FilePath -> Task m s -> PathTree -> Rules ()
runRecTask buildPath task@Task{..} pathTree@(Dir _ _) = do
    let (Dir rootDir subTrees) = filterFTree (\f -> takeExtension f == "." ++ extension) pathTree
    let files = filterFiles subTrees
    let dirs  = filterDirs subTrees

    runTask task buildPath rootDir files $ map (\(Dir x _) -> x) dirs

    forM_ dirs $ runRecTask buildPath task

runRecTask _ _ _ = error "Impossible happens"

runTask :: Task meta s -> FilePath -> FilePath -> [FilePath] -> [FilePath] -> Rules ()
runTask Task{..} buildPath rootDir files dirPaths = do
    want [buildPath </> rootDir </> "index.html"]
    want $ map (\f -> buildPath </> f -<.> ".html") files

    getMS <- newCache' $ do
        need files
        texts <- mapM readFile' files
        gitDates <- mapM getGitDate files
        let metas = catMaybes $ map (\(f, d, t) -> extract f d t) (zip3 files gitDates texts)
        let filenames = map (dropExtension . takeFileName) files
        let metaMap = HM.fromList $ zip filenames metas
        let summary = summarize rootDir dirPaths metaMap
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

  where
    getGitDate p = do
        let gitCmd = "git log -1 --format=%ci --" :: String
        Stdout gitDate <- cmd gitCmd [unpack p]
        return (pack gitDate)


