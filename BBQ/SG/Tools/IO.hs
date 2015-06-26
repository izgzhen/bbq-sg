module BBQ.SG.Tools.IO (
  prepareFolders   
, withMarkdownsAll
, withPage
, syncImages
, getJsCSS
) where

import BBQ.SG.Misc
import Data.Set (fromList, intersection, difference, toList)
import BBQ.SG.Config
import BBQ.SG.Meta
import BBQ.SG.Tools.Parser
import BBQ.SG.Tools.AutoKeywords
import Text.Blaze.Html.Renderer.Text
import Data.Text.Lazy (Text, pack)
import System.Directory
import System.FilePath
import Control.Applicative((<$>))
import Control.Monad
import Text.Blaze.Html5 (Html)
import Prelude hiding (writeFile)
import qualified Data.Map as M
import Data.Text.Lazy.IO (writeFile)

prepareFolders config = mapM_ (createDirectoryIfMissing True)
                            $ map (\f -> f config)
                                  [ _postsDir
                                  , _imgStaDir
                                  , _jsStaDir
                                  , _cssStaDir
                                  ]

withMarkdownsAll :: Config -> ((Text, Meta) -> M.Map FilePath Int -> Html) -> IO [Meta]
withMarkdownsAll config processor = do
    print "Generating posts..."

    filenames <- map dropExtensions <$> getFilesEndWith markdownDir ".md"

    contents  <- mapM (\filename -> readFileMaybe $ markdownDir </> filename ++ ".md") filenames

    keywordsGroup <- generateKeyWords $ _markdownDir config

    case foldr (withFile $ M.fromList keywordsGroup) (Right []) (zip contents filenames) of
        Left errMsg      -> do print $ "Error: " ++ errMsg
                               return []
        Right collection -> do
            mapM_ (\(html, filename) -> writeFile (postsDir </> filename ++ ".html") (renderHtml html))
                 $ zip (map snd collection) filenames
            return $ map fst collection
  where
    markdownDir = _markdownDir config
    postsDir    = _postsDir    config
    withFile :: M.Map FilePath (M.Map String Int) -> (EitherS String, FilePath) -> EitherS [(Meta, Html)] -> EitherS [(Meta, Html)]
    withFile keywordsGroup (maybeContent, path) mPairs = do
        pairs        <- mPairs
        content      <- maybeContent
        (Meta_ t d a tg _, str') <- parseMeta content
        let meta = Meta_ t d a tg $ "posts" </> path ++ ".html"
        let mdpath = markdownDir </> path ++ ".md"
        let Just keywords = M.lookup mdpath keywordsGroup
        return $ (meta, processor (pack str', meta) keywords) : pairs

withPage name config f = do
    print $ "Generating page " ++ name ++ " ..."
    html <- f
    writeFile (_staticDir config </> name ++ ".html") (renderHtml html)

syncImages config = do
    print "Sync images ..."
    syncResource (_imgSrcDir config) (_imgStaDir config) (_staticDir config)

syncJs config = do
    print "Sync JavaScripts ..."
    syncResource (_jsSrcDir config) (_jsStaDir config) (_staticDir config)

syncCss config = do
    print "Sync CSS ..."
    syncResource (_cssSrcDir config) (_cssStaDir config) (_staticDir config)


getJsCSS config = do
    syncJs config
    syncCss config
    js  <- getFilesEndWith (_jsSrcDir config) ".js"
    css <- getFilesEndWith (_cssSrcDir config) ".css"
    return (js, css)

syncResource srcDir staDir prefix = do

    src    <- fromList . map fst <$> getFileDict srcDir
    static <- fromList . map (dropFirstDir prefix) . map fst <$> getFileDict staDir

    let notInSrc = toList $ difference static src
    let notInSta = toList $ difference src static
    mapM_ (\invalid -> do
                print $ "remove invalid " ++ show invalid
                removeFile $ invalid
          ) notInSrc
    mapM_ (\new     -> do
                print $ "add new " ++ show new
                copyFile new (prefix </> new)
          ) notInSta

    let common = toList $ intersection src static

    mapM_ (\commonPath -> do
                srcSize <- getFileSize commonPath
                staSize <- getFileSize (prefix </> commonPath)
                -- srcMod  <- getModificationTime commonPath
                -- staMod  <- getModificationTime (_staticDir config </> commonPath)

                if srcSize /= staSize then do
                        print $ "updating " ++ show (prefix </> commonPath) ++ " with " ++ show commonPath
                        copyFile commonPath (prefix </> commonPath)
                    else return ()
          ) common




