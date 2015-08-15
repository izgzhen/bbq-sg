module BBQ.SG.Components.Wiki (wikiGen) where
import BBQ.SG.Tools.IO
import BBQ.SG.Template
import BBQ.SG.Plugin
import BBQ.SG.Misc
import System.FilePath((</>), FilePath, takeExtensions, dropExtensions)
import Text.Markdown
import Data.Text.Lazy (Text, pack, unpack)
import BBQ.SG.Config
import Control.Applicative
import Text.Blaze.Html5 (toHtml)

{-
    Any link will direct to a node, a node could be
    folder node or text node.
    
    For a folder, if it has a same-name sub-file,
    the content will be displayed as well as the
    menu.

    Or, it is not a folder, we will name it as a
    text node, a text node is simply its text and the menu.
    
-}


    

wikiGen headers config layout = go (_wikiSrc config) (_wikiURL config) renderPage
    where
        go path url renderPage = do
            files <- filter (\p -> takeExtensions p == ".md") <$> getSubFiles path
            dirs  <- filter (\p -> head (dropParent p) /= '.') <$> getSubDirs path
            -- print files
            -- print dirs
            let filenames = map dropParent files
            let dirnames  = map dropParent dirs
            let pathname  = dropParent path

            let commmonFiles = zip filenames (map (\p -> dropExtensions p ++ ".html") filenames)
            let commonDirs   = zip dirnames  (map (\p -> p </> p ++ ".html") dirnames)
            let urlList = [(pathname, pathname ++ ".html")] ++ commmonFiles ++ commonDirs

            -- print "render the index page"
            if (pathname ++ ".md") `elem` filenames then
                renderPage pathname urlList (Just $ path </> pathname ++ ".md") (url </> pathname)
                else renderPage pathname urlList Nothing (url </> pathname)

            -- print "render common page"
            mapM_ (\(p, n) -> renderPage (dropParent p) urlList (Just p) (url </> dropExtensions n)) $ zip files filenames

            -- print "recursively render directories"
            mapM_ (\d -> go d (url </> dropParent d) renderPage) dirs

        renderPage :: String -> [(String, FilePath)] -> Maybe FilePath -> FilePath -> IO ()
        renderPage title menulist maybeMarkdown url = do
            -- print $ "rendering " ++ url
            case maybeMarkdown of
                Just filepath -> do
                    eitherText <- readFileMaybe filepath
                    case eitherText of
                        Right text -> do
                            let html = markdown def (pack text)
                            let html' = layout title menulist (Just html)
                            let html'' = htmlTemplate title headers html'
                            withPage url config html''
                            return ()
                Nothing -> do
                    let html = layout title menulist Nothing
                    let html' = htmlTemplate title headers html
                    withPage url config html'
                    return ()
