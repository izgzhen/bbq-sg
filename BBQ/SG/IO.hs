module BBQ.SG.IO (
    withMarkdownAll,
    withIndex,
    genTagsIndex,
    FilePath(..),
    syncImages
) where

import BBQ.SG.Config
import BBQ.SG.Meta
import BBQ.SG.More
import System.Directory
import System.FilePath (FilePath(..), (</>), takeExtensions, dropExtensions)
import Text.Blaze.Html.Renderer.Text
import Data.Text.Lazy (pack)
import Control.Monad
import Control.Applicative((<$>))
import Data.Set (fromList, intersection, difference, toList)
import Text.Blaze.Html5 (Html)
import System.Posix
import Data.List.Extra(splitOn)

readMarkdownFile path = do
    exist <- doesFileExist (path ++ ".md")
    if exist then do
        text <- readFile (path ++ ".md")
        return $ Right text
        else return $ Left ("Reading " ++ show path ++ " failed")


getFileList path = do
  names <- getDirectoryContents path
  return $ map dropExtensions $ filter (\name -> takeExtensions name == ".md") names

withMarkdownAll config f = do
    print "Generating posts..."
    filenames <- getFileList markdownDir

    contents  <- mapM (\filename ->readMarkdownFile $ markdownDir </> filename) filenames
    case foldr withFile (Right []) (zip contents (map (\fn -> "posts" </> fn ++ ".html") filenames)) of
        Left errMsg     -> do print $ "Error: " ++ errMsg
                              return []
        Right collection -> do
            createDirectoryIfMissing True postsDir
            mapM_ (\(html, filename) -> writeHtmlFile (postsDir </> filename) (renderHtml html))
                 $ zip (map snd collection) filenames
            return $ map fst collection
  where
    markdownDir = _markdownDir config
    postsDir    = _postsDir    config
    withFile :: (EitherS String, FilePath) -> EitherS [(Meta, Html)] -> EitherS [(Meta, Html)]
    withFile (maybeContent, path) mPairs = do
        pairs        <- mPairs
        content      <- maybeContent
        (Meta_ t d a tg _, str') <- parseMeta content
        let meta = Meta_ t d a tg path
        return $ (meta, f (pack str', meta)) : pairs

withIndex config f = do
    print "Generating index..."
    let staticDir = _staticDir config
    let markdownDir = _markdownDir config
    createDirectoryIfMissing True staticDir
    markdowns <- getFileList $ markdownDir
    html <- f
    writeHtmlFile (staticDir </> "index") (renderHtml html)


syncImages config = do
    print "Sync images ..."
    let imgSrcDir = _imgSrcDir config
    let imgStaDir = _imgStaDir config
    imagesSrc <- fromList . map fst <$> getFileDict imgSrcDir
    imagesSta <- fromList . map (dropFirstDir $ _staticDir config) . map fst <$> getFileDict imgStaDir

    let notInSrc = toList $ difference imagesSta imagesSrc
    let notInSta = toList $ difference imagesSrc imagesSta
    mapM_ (\invalid -> do
                print $ "remove invalid " ++ show invalid
                removeFile $ invalid
          ) notInSrc
    mapM_ (\new     -> do
                print $ "add new " ++ show new
                copyFile new (_staticDir config </> new)
          ) notInSta

    let common = toList $ intersection imagesSta imagesSrc

    mapM_ (\commonPath -> do
                srcSize <- getFileSize commonPath
                staSize <- getFileSize (_staticDir config </> commonPath)
                -- srcMod  <- getModificationTime commonPath
                -- staMod  <- getModificationTime (_staticDir config </> commonPath)

                if srcSize /= staSize then do
                        print $ "updating " ++ show (_staticDir config </> commonPath) ++ " with " ++ show commonPath
                        copyFile commonPath (_staticDir config </> commonPath)
                    else return ()
          ) common

getFileSize path = getFileStatus path >>= \s -> return $ fileSize s

getFileDict path = do
    contents <- (filter (\n -> n /= "." && n /= "..")) <$> getDirectoryContents path
    let contentsDict = zip (map (path </>) contents) contents
    dirs     <- filterM doesDirectoryExist $ map fst contentsDict
    files    <- filterM (doesFileExist . fst) contentsDict
    dict     <- concat <$> mapM getFileDict dirs
    return $ dict ++ files


-- example: "./static/img" -> "./img" with "./static" to drop
dropFirstDir prefix str   = "." ++ (head . tail $ splitOn prefix str)
