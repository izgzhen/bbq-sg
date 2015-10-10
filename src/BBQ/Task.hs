module BBQ.Task (
  ReadTask(..)
, WriteTask(..)
, Build
, runBuild
, Task(..)
, runReadTask
, runBatchWriteTask
, runSimpleWriteTask
, runTask
, emptyCont
) where

import BBQ.Import hiding (toList)
import qualified Data.HashMap.Lazy as HM
import Data.Foldable (toList)

data ReadTask  x = ReadTask  (Text -> Text -> FilePath -> Build x)

data WriteTask x = WriteTask (x -> Build (FilePath, Text)) [FilePath]
                 | VoidWriteTask

type Build = ReaderT BuildConfig (Except Text)
runBuild b config = let Identity ret = runExceptT $ runReaderT b config in ret

data Task meta summary extra widget = Task {
    extract         :: ReadTask meta,
    summarize       :: summary -> meta -> Build summary,
    render          :: WriteTask (meta, extra),
    relate          :: summary -> meta -> Build extra,
    buildWidget     :: Maybe (summary -> (Text, widget)),
    mEmerge         :: Maybe (WriteTask (summary, [FilePath], FilePath)), -- subdirs and current dir
    initialSummary  :: summary
}

-------------------------- / Runner / -------------------------------------

runReadTask :: (Traversable t, Foldable t) => BuildConfig -> t FilePath -> ReadTask x -> Action (Build (t x))
runReadTask config sources (ReadTask extract) = do
    need $ toList sources
    metas <- forM sources (\src -> do
                text <- readFile' src
                date <- getGitDate src
                return $ extract text date src)
    return $ sequence metas

runBatchWriteTask :: (Functor f, MonoFoldable c, MonoFoldable (f (Either Text (FilePath, Text))),
                     Element (f (Either Text (FilePath, Text))) ~ Either Text t1,
                     Element (f (Either Text (FilePath, Text))) ~ Either t2 (FilePath, c),
                     Element c ~ Char) =>
                     BuildConfig -> f t -> WriteTask t -> Action ()
runBatchWriteTask config xs (WriteTask builder deps) = do
    let builds = fmap builder xs
    let outputs = fmap (\b -> runBuild b config) builds
    need deps
    forM_ outputs $ \eResult -> case eResult of
        Left errMsg      -> error_ $ "ERROR in write task: " ++ errMsg
        Right (fp, text) -> writeFile' fp text
runBatchWriteTask _ _ VoidWriteTask = return ()

runSimpleWriteTask :: BuildConfig -> x -> WriteTask x -> Action ()
runSimpleWriteTask config x (WriteTask builder deps) = do
    need deps
    case runBuild (builder x) config of
        Left errMsg      -> error_ $ "ERROR in collect task: " ++ errMsg
        Right (fp, text) -> writeFile' fp text

runTask :: BuildConfig -> FilePath -> Task m s e w -> (BuildConfig -> FilePath -> Action ()) -> Action (Maybe (Text, w))
runTask config rootDir task@Task{..} continuation = do
    files <- getDirectoryFiles "" [ rootDir </> "*.md" ] -- Current level
    dirs  <- getDirectoryDirs rootDir
    metasbuild <- runReadTask config files extract
    case runBuild (b metasbuild) config of
        Left errMsg -> do
            error_ $ "Error in runTask: " ++ errMsg
            return Nothing
        Right (pairs, mWidget, summary) -> do
            runBatchWriteTask config pairs render
            case mEmerge of
                Nothing -> return ()
                Just emerge -> runSimpleWriteTask config (summary, dirs, rootDir) emerge 
            forM_ dirs (\d -> continuation config d)
            return mWidget
    where
        b metasbuild = do
            metas   <- metasbuild
            summary <- foldM summarize initialSummary metas
            extras  <- sequence $ fmap (relate summary) metas
            return (zip metas extras, ($ summary) <$> buildWidget, summary)



emptyCont _ _ = return ()
