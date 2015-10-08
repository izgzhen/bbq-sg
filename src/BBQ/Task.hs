module BBQ.Task where

import BBQ.Import

data ReadTask  x = ReadTask  (Text -> Text -> FilePath -> Build x)
data WriteTask x = WriteTask (Text -> x -> Build (FilePath, Text)) [FilePath]

newtype URL = URL FilePath

type Build = ReaderT BuildConfig (Except Text)
runBuild b config = let Identity ret = runExceptT $ runReaderT b config in ret

runReadTask :: BuildConfig -> String -> ReadTask x -> Action (Build [x])
runReadTask config out (ReadTask extract) = do
    let src = dropExtension (dropDirectory1 out) ++ ".md"
    need [src]
    markdowns <- getDirectoryFiles "" [src] :: Action [String]
    texts <- mapM readFile' markdowns
    dates <- mapM getGitDate markdowns
    let metas = fmap (\(x, y, z) -> extract x y z) (zip3 texts dates markdowns)
    return $ sequence metas


runWriteTask :: BuildConfig -> [(meta, extra)] -> WriteTask (meta, extra) -> Action ()
runWriteTask config pairs (WriteTask builder deps) = do
    let template = "" -- should need templates 
    let builds = map (builder template) pairs
    let outputs = map (\b -> runBuild b config) builds
    need deps
    forM_ outputs $ \eResult -> case eResult of
        Left errMsg      -> error_ errMsg
        Right (fp, text) -> writeFile' fp text

data Task meta summary extra = Task {
    extract   :: ReadTask meta,
    summarize :: summary -> meta -> Build summary,
    render    :: WriteTask (meta, extra),
    relate    :: summary -> meta -> Build extra,
    initialSummary :: summary
}

runTask :: FilePath -> Task m s e -> BuildConfig -> Action ()
runTask fp (Task rt s wt r is) config = do
    metas <- runReadTask config fp rt
    case runBuild (b metas) config of
        Left errMsg -> error_ errMsg
        Right pairs -> runWriteTask config pairs wt
    where
        b metas' = do
            metas <- metas'
            sm <- foldM s is metas
            extras <- sequence $ map (r sm) metas
            return $ zip metas extras


askBuild :: Build BuildConfig
askBuild = ask


