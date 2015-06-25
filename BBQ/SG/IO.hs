module BBQ.SG.IO (
    withMarkdownAll,
    withIndex,
    FilePath(..)
) where

import BBQ.SG.Config
import BBQ.SG.Meta
import System.Directory(doesFileExist, getDirectoryContents, copyFile, createDirectoryIfMissing)
import System.FilePath (FilePath(..), (</>), takeExtensions, dropExtensions)
import Data.Text.Lazy.IO (writeFile)
import Prelude hiding (writeFile)
import Text.Blaze.Html.Renderer.Text
import Data.Text.Lazy (pack)
import Control.Monad (foldM)
import Text.Blaze.Html5 (Html)

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


withMarkdownAll config f = do
    filenames <- getFileList markdownDir
    contents  <- mapM (\filename ->readMarkdownFile $ markdownDir </> filename) filenames
    case foldr withFile (Just []) contents of  
        Nothing         -> do print "not good"
                              return []
        Just collection -> do
            createDirectoryIfMissing True postsDir
            mapM_ (\(html, filename) -> writeHtmlFile (postsDir </> filename) (renderHtml html))
                 $ zip (map snd collection) filenames
            return $ map fst collection
  where
    markdownDir = _markdownDir config
    postsDir    = _postsDir    config
    withFile :: (Maybe String) -> Maybe [(Meta, Html)] -> Maybe [(Meta, Html)]
    withFile maybeContent mPairs = do -- Maybe
        pairs        <- mPairs
        content      <- maybeContent
        (meta, str') <- parseMeta content
        return $ (meta, f (pack str', meta)) : pairs

withIndex config f = do
    let staticDir = _staticDir config
    let markdownDir = _markdownDir config
    createDirectoryIfMissing True staticDir
    markdowns <- getFileList $ markdownDir
    html <- f $ zip markdowns (map (\name -> "posts" </> name ++ ".html") markdowns)
    writeHtmlFile (staticDir </> "index") (renderHtml html)

-- Only a stub
-- syncImages = copyFile imgSrcDir imgStaDir
