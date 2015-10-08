module BBQ.Task where

-- import Control.Monad.Reader
import BBQ.Import
import Development.Shake hiding (readFile', writeFile')
import BBQ.IO
import Development.Shake.FilePath

data ReadTask  x = ReadTask  (Text -> Text -> Text -> x)
data WriteTask x = WriteTask (Text -> x -> Build (FilePath, Text)) [FilePath]

type Markdown = Text
newtype URL = URL Text

type Build = Reader BuildConfig

runReadTask :: String -> ReadTask x -> Action [x]
runReadTask out (ReadTask extract) = do
    let src = dropExtension (dropDirectory1 out) ++ ".md"
    need [src]
    markdowns <- getDirectoryFiles "" [src]
    texts <- mapM readFile' markdowns
    dates <- mapM (\p -> do
                    let gitCmd = "git log -1 --format=%ci --" :: String
                    Stdout gitDate <- cmd gitCmd [p]
                    return (pack gitDate))
                  markdowns
    let metas = fmap (\(x, y, z) -> extract x y z) (zip3 texts dates $ fmap pack markdowns)
    return metas

runWriteTask :: BuildConfig -> [(meta, extra)] -> WriteTask (meta, extra) -> Action ()
runWriteTask config pairs (WriteTask builder deps) = do
    let template = "" -- should need templates 
    let builds = map (builder template) pairs
    let outputs = map (flip runReaderT config) builds
    need deps
    forM_ outputs $ \(Identity (fp, text)) -> do
        writeFile' fp text

data Task meta summary extra = Task {
    extract   :: ReadTask meta,
    summarize :: summary -> meta -> summary,
    render    :: WriteTask (meta, extra),
    relate    :: summary -> meta -> extra,
    initialSummary :: summary
}

runTask :: FilePath -> Task m s e -> BuildConfig -> Action ()
runTask fp (Task rt s wt r is) config = do
    metas <- runReadTask fp rt
    let sm = foldl' s is metas
    let extras = map (r sm) metas
    runWriteTask config (zip metas extras) wt


askBuild :: Build BuildConfig
askBuild = ask


