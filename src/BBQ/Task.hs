module BBQ.Task where

import BBQ.Import hiding (toList)
import qualified Data.HashMap.Lazy as HM
import Data.Tree
import Data.Foldable (toList)

data ReadTask  x = ReadTask  (Text -> Text -> FilePath -> Build x)

data WriteTask x = WriteTask (x -> Build (FilePath, Text)) [FilePath]
                 | VoidWriteTask

type Build = ReaderT BuildConfig (Except Text)
runBuild b config = let Identity ret = runExceptT $ runReaderT b config in ret

data Task meta summary extra widget = Task {
    extract     :: ReadTask meta,
    summarize   :: summary -> meta -> Build summary,
    render      :: WriteTask (meta, extra),
    relate      :: summary -> meta -> Build extra,
    buildWidget :: Maybe (summary -> (Text, widget)),
    initialSummary :: summary
}

-------------------------- / Runner / -------------------------------------

runReadTask :: (Traversable t, Foldable t) => BuildConfig -> t FilePath -> ReadTask x -> Action (Build (t x))
runReadTask config markdowns (ReadTask extract) = do
    need $ toList markdowns
    metas <- forM markdowns (\m -> do
                text <- readFile' m
                date <- getGitDate m
                return $ extract text date m)
    return $ sequence metas

runWriteTask config xs (WriteTask builder deps) = do
    let builds = fmap builder xs
    let outputs = fmap (\b -> runBuild b config) builds
    need deps
    forM_ outputs $ \eResult -> case eResult of
        Left errMsg      -> error_ $ "ERROR in write task: " ++ errMsg
        Right (fp, text) -> writeFile' fp text
runWriteTask _ _ VoidWriteTask = return ()

runCollectTask :: BuildConfig -> HashMap Text widget -> WriteTask (HashMap Text widget) -> Action ()
runCollectTask config hm (WriteTask builder deps) = do
    need deps
    case runBuild (builder hm) config of
        Left errMsg      -> error_ $ "ERROR in collect task: " ++ errMsg
        Right (fp, text) -> writeFile' fp text

runTask src (Task rt s wt r buildWidget is) mkEmerge emerger config = do
    metas <- runReadTask config src rt
    case runBuild (b metas) config of
        Left errMsg -> do
            error_ $ "ERROR in runTask: " ++ errMsg
            return Nothing
        Right (pairs, mWidget, emerge) -> do
            runWriteTask config pairs wt
            runWriteTask config emerge emerger
            return mWidget
    where
        b metas' = do
            metas <- metas'
            summary <- foldM s is metas
            let emerge = mkEmerge summary
            extras <- sequence $ fmap (r summary) metas
            return (zip metas extras, ($ summary) <$> buildWidget, emerge)


