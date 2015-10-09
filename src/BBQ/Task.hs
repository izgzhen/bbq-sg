module BBQ.Task where

import BBQ.Import

data ReadTask  x = ReadTask  (Text -> Text -> FilePath -> Build x)
data WriteTask x = WriteTask (x -> Build (FilePath, Text)) [FilePath]

type Widget  = HtmlUrl WebPath
type Widgets = HashMap Text Widget

type Build = ReaderT BuildConfig (Except Text)
runBuild b config = let Identity ret = runExceptT $ runReaderT b config in ret


runReadTask :: BuildConfig -> String -> ReadTask x -> Action (Build [x])
runReadTask config out (ReadTask extract) = do
    let src = dropDirectory1 out -<.> ".md"
    need [src]
    markdowns <- getDirectoryFiles "" [src] :: Action [String]
    texts <- mapM readFile' markdowns
    dates <- mapM getGitDate markdowns
    let metas = fmap (\(x, y, z) -> extract x y z) (zip3 texts dates markdowns)
    return $ sequence metas


runWriteTask :: BuildConfig -> [x] -> WriteTask x -> Action ()
runWriteTask config xs (WriteTask builder deps) = do
    let builds = map builder xs
    let outputs = map (\b -> runBuild b config) builds
    need deps
    forM_ outputs $ \eResult -> case eResult of
        Left errMsg      -> error_ $ "ERROR in write task: " ++ errMsg
        Right (fp, text) -> writeFile' fp text

runCollectTask :: BuildConfig -> HashMap Text x -> WriteTask (HashMap Text x) -> Action ()
runCollectTask config hm (WriteTask builder deps) = do
    need deps
    case runBuild (builder hm) config of
        Left errMsg      -> error_ $ "ERROR in collect task: " ++ errMsg
        Right (fp, text) -> writeFile' fp text

data Task meta summary extra = Task {
    extract     :: ReadTask meta,
    summarize   :: summary -> meta -> Build summary,
    render      :: WriteTask (meta, extra),
    relate      :: summary -> meta -> Build extra,
    buildWidget :: Maybe (summary -> (Text, Widget)),
    initialSummary :: summary
}

runTask :: FilePath -> Task m s e -> BuildConfig -> Action (Maybe (Text, Widget))
runTask fp (Task rt s wt r bw is) config = do
    metas <- runReadTask config fp rt
    case runBuild (b metas) config of
        Left errMsg -> do
            error_ $ "ERROR in runTask: " ++ errMsg
            return Nothing
        Right (pairs, mWidget) -> do
            runWriteTask config pairs wt
            return mWidget
    where
        b metas' = do
            metas <- metas'
            sm <- foldM s is metas
            extras <- sequence $ map (r sm) metas
            return (zip metas extras, fmap ($ sm) bw)

