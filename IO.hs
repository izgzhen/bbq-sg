module IO (
    withMarkdownAll,
    withIndex
) where

import Config
import System.Directory(doesFileExist, getDirectoryContents)
import System.FilePath ((</>), takeExtensions, dropExtensions)
import Data.Text.Lazy.IO (writeFile)
import Prelude hiding (writeFile)
import Data.Text.Lazy (pack)
import Text.Blaze.Html.Renderer.Text

readMarkdownFile path = do
    exist <- doesFileExist (path ++ ".md")
    if exist then do
        text <- readFile (path ++ ".md")
        return $ Just text
        else return Nothing

writeHtmlFile title = writeFile (title ++ ".html")


getFileList path = do
  names <- getDirectoryContents path
  return $ map dropExtensions $ filter (\name -> takeExtensions name == ".md") names

withMarkdownAll f = do
    filenames <- getFileList markdownDir
    mapM_ withFileName filenames

  where
    withFileName filename = do
        Just str <- readMarkdownFile $ markdownDir </> filename
        html <- f (pack str, filename)
        writeHtmlFile (staticDir </> filename) (renderHtml html)

withIndex f = do
    markdowns <- getFileList $ markdownDir
    html <- f $ zip markdowns (map (\name -> name ++ ".html") markdowns)
    writeHtmlFile (staticDir </> "index") (renderHtml html)


