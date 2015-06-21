module IO (
    writeHtmlFile,
    readMarkdownFile
) where

import System.Directory(doesFileExist)
import Data.Text.Lazy.IO (writeFile)
import Prelude hiding (writeFile)

readMarkdownFile path = do
    exist <- doesFileExist (path ++ ".md")
    if exist then do
        text <- readFile (path ++ ".md")
        return $ Just text
        else return Nothing

writeHtmlFile title = writeFile (title ++ ".html")