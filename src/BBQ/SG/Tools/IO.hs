module BBQ.SG.Tools.IO (
  prepareFolders   
, withPostMarkdowns
, withPage
, syncImages
, syncJs
, syncCss
, getJsCSS
) where

import BBQ.SG.Misc
import Data.Set (fromList, intersection, difference, toList)
import BBQ.SG.Config
import BBQ.SG.Meta
import BBQ.SG.Tools.Parser
import BBQ.SG.Tools.AutoKeywords
import BBQ.SG.Tools.Synopsis
import Text.Blaze.Html.Renderer.Text
import Data.Text.Lazy (Text, pack, unpack)
import System.Directory
import System.FilePath
import Control.Applicative((<$>))
import Control.Monad
import Text.Blaze.Html5 (Html)
import Prelude hiding (writeFile)
import qualified Data.Map as M
import Data.Text.Lazy.IO (writeFile)

prepareFolders config = do mapM_ (createDirectoryIfMissing True)
                            $ map (\f -> f config)
                                  [ _postsSta
                                  , _pageSta
                                  , _imgSta
                                  , _jsSta
                                  , _cssSta
                                  , _tagsSta
                                  ]



withPostMarkdowns :: Config -> ((Text, Meta) -> Synopsis -> [(FilePath, Int)] -> Html) -> IO [Meta]
withPostMarkdowns config processor = do
    putStrLn "Generating posts..."

    filenames <- map dropExtensions <$> getFilesEndWith (_postsSrc config) ".md"

    contents  <- mapM (\filename -> readFileMaybe $ (_postsSrc config) </> filename ++ ".md") filenames

    keywordsGroup <- generateKeyWords config

    case foldr (withFile $ M.fromList keywordsGroup) (Right []) (zip contents filenames) of
        Left errMsg      -> error $ "Error: " ++ errMsg
        Right collection -> do
            mapM_ (\(html, filename) -> writeFile (_postsSta config </> filename ++ ".html") (renderHtml html))
                 $ zip (map snd collection) filenames
            return $ map fst collection
  where
    withFile :: M.Map FilePath (M.Map String Int) -> (EitherS String, FilePath) -> EitherS [(Meta, Html)] -> EitherS [(Meta, Html)]
    withFile keywordsGroup (maybeContent, path) mPairs = do
        pairs        <- mPairs
        content      <- maybeContent
        (Meta_ t d a tg _, str') <- parseMeta content
        let meta = Meta_ t d a tg $ "posts" </> path ++ ".html"
        let (synopsis, body') = extract str'
        let mdpath = (_postsSrc config) </> path ++ ".md"
        let Just keywords = M.lookup mdpath keywordsGroup
        return $ (meta, processor (pack body', meta) synopsis (M.toList keywords)) : pairs

-- Generate by URL
withPage url config html = do
    debugPrint config $ "Generating page " ++ url ++ " ..."
    writeFileRobust (_staticDir config </> url ++ ".html") (renderHtml html)

syncImages config = do
    putStrLn "Sync images ..."
    syncResource (_imgSrc config) (_imgSta config) (_srcDir config) (_staticDir config)

syncJs config = do
    putStrLn "Sync JavaScripts ..."
    syncResource (_jsSrc config) (_jsSta config)  (_srcDir config) (_staticDir config)

syncCss config = do
    putStrLn "Sync CSS ..."
    syncResource (_cssSrc config) (_cssSta config)  (_srcDir config) (_staticDir config)


getJsCSS config = do
    syncJs config
    syncCss config
    js  <- getFilesEndWith (_jsSrc config) ".js"
    css <- getFilesEndWith (_cssSrc config) ".css"
    return (js, css)

syncResource srcDir staDir srcRoot staRoot = do

    src    <- fromList . filterJust . map (dropPrefix srcRoot . fst) <$> getFileDict srcDir
    static <- fromList . filterJust . map (dropPrefix staRoot . fst) <$> getFileDict staDir

    let notInSrc = toList $ difference static src
    let notInSta = toList $ difference src static
    mapM_ (\invalid -> do
                print $ "remove invalid " ++ show (staRoot </> invalid)
                removeFile (staRoot </> invalid)
          ) notInSrc
    mapM_ (\new     -> do
                print $ "add new " ++ show (staRoot </> new)
                copyFileRobust (srcRoot </> new) (staRoot </> new)
          ) notInSta

    let common = toList $ intersection src static

    mapM_ (\commonPath -> do
            srcSize <- getFileSize (srcRoot </> commonPath)
            staSize <- getFileSize (staRoot </> commonPath)

            if srcSize /= staSize then do
                    print $ "updating " ++ show (staRoot </> commonPath) ++ " with " ++ show (srcRoot </> commonPath)
                    copyFileRobust (srcRoot </> commonPath) (staRoot </> commonPath)
                else return ()
          ) common




