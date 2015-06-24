module IO (
    withMarkdownAll,
    withIndex
) where

import Meta
import Config
import System.Directory(doesFileExist, getDirectoryContents, copyFile, createDirectoryIfMissing)
import System.FilePath ((</>), takeExtensions, dropExtensions)
import Data.Text.Lazy.IO (writeFile)
import Prelude hiding (writeFile)
import Text.Blaze.Html.Renderer.Text
import Data.Text.Lazy (pack)

readMarkdownFile path = do
    exist <- doesFileExist (path ++ ".md")
    if exist then do
        text <- readFile (path ++ ".md")
        return $ Just text
        else return Nothing

writeHtmlFile filename = writeFile (filename ++ ".html")

getFileList path = do
  names <- getDirectoryContents path
  return $ map dropExtensions $ filter (\name -> takeExtensions name == ".md") names

-- foldM :: forall a b (m :: * -> *). Monad m => (a -> b -> m a) -> a -> [b] -> m a
-- b: filename
-- a: MetaInfos

withMarkdownAll f = do
    filenames <- getFileList markdownDir
    mapM_ withFileName filenames
    createDirectoryIfMissing True postsDir
  where
    withFileName filename = do
        Just str <- readMarkdownFile $ markdownDir </> filename
        case parseMeta str of
            Just (meta, str') -> do
                html <- f (pack str', meta)
                writeHtmlFile (postsDir </> filename) (renderHtml html)
            Nothing           -> print $ "parsing metainfo of " ++ filename ++ " failed, not written!"

withIndex f = do
    createDirectoryIfMissing True staticDir
    markdowns <- getFileList $ markdownDir
    html <- f $ zip markdowns (map (\name -> "posts" </> name ++ ".html") markdowns)
    writeHtmlFile (staticDir </> "index") (renderHtml html)

-- Only a stub
syncImages = copyFile imgSrcDir imgStaDir
