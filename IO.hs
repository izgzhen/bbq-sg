module IO (
    withFileName
) where

import System.Directory(doesFileExist)
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

withFileName filename f = do
    Just str <- readMarkdownFile filename
    html <- f $ pack str
    writeHtmlFile filename (renderHtml html)
