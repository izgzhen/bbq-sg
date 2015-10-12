module BBQ.Task (
  Task(..)
, Render(..)
, Collector(..)
, runRecTask
, runTask
, runCollectTask
) where

import BBQ.Import
import qualified Data.HashMap.Lazy as HM
import BBQ.FTree

data Task meta s = Task {
    extension    :: FilePath,
    extract      :: FilePath -> Text -> Text -> Maybe meta,
    summarize    :: FilePath -> [FilePath] -> HashMap FilePath meta -> s, -- task implementor can choose to ignore the second parameter
    -- renderIndex  :: BuildConfig -> s -> Action Text,
    -- renderPage   :: BuildConfig -> s -> meta -> Action Text, -- Source path, rendered text
    renderWidget :: s -> Action Text
}

-- Render should be provided by the user
data Render meta s = Render {
    renderIndex  :: s -> Action Text,
    renderPage   :: s -> meta -> Action Text -- Source path, rendered text
}

data Collector = Collector {
    target   :: FilePath,
    widgets  :: [FilePath],
    resolver :: HashMap FilePath Text -> Maybe Text
}

runRecTask :: FilePath -> Task meta s -> Render meta s -> PathTree -> Rules ()
runRecTask buildPath task@Task{..} render pathTree@(Dir _ _) = do
    let (Dir rootDir subTrees) = filterFTree (\f -> takeExtension f == "." ++ extension) pathTree
    let files = filterFiles subTrees
    let dirs  = filterDirs subTrees

    runTask task render buildPath rootDir files $ map (\(Dir x _) -> x) dirs

    forM_ dirs $ runRecTask buildPath task render

runRecTask _ _ _ _ = error "Impossible happens"

runTask :: Task meta s -> Render meta s -> FilePath -> FilePath -> [FilePath] -> [FilePath] -> Rules ()
runTask Task{..} Render{..} buildPath rootDir files dirPaths = do
    want [buildPath </> rootDir </> "index.html"]
    want [buildPath </> rootDir </> "widget.json"]
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

    buildPath </> rootDir </> "widget.json" %> \out -> do
        (_, summary) <- getMS
        t <- renderWidget summary
        writeFile' out t

  where
    getGitDate p = do
        let gitCmd = "git log -1 --format=%ci --" :: String
        Stdout gitDate <- cmd gitCmd [unpack p]
        return (pack gitDate)


runCollectTask :: FilePath -> Collector -> Rules ()
runCollectTask buildPath Collector{..} = do
    want [buildPath </> target]
    let widgets' = map (buildPath </>) widgets
    buildPath </> target %> \out -> do
        need widgets'
        texts <- mapM readFile' widgets'
        case resolver $ HM.fromList $ zip widgets texts of
            Nothing   -> putNormal $ "can't build " ++ target
            Just text -> writeFile' out text

