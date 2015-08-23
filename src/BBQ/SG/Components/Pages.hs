{-# LANGUAGE OverloadedStrings #-}

module BBQ.SG.Components.Pages (pagesGen) where
import BBQ.SG.Tools.IO
import System.FilePath ((</>), dropExtensions)
import BBQ.SG.Template

import BBQ.SG.Misc
import Control.Applicative((<$>))
import Control.Monad(mapM_)
import BBQ.SG.Config
import Text.Markdown
import Data.Text.Lazy (pack)

pagesGen config meta (layout, resources) = do
    let headers = map (resourceToHeader config) resources
    putStrLn "Generating pages..."

    filenames <- map dropExtensions <$> getFilesEndWith (_pageSrc config) ".md"

    debugPrint config $ "pages: " ++ show filenames

    contents  <- mapM (\filename -> readFileMaybe $ (_pageSrc config) </> filename ++ ".md") filenames

    let htmls = map (\(Right x) -> markdown def (pack x)) contents

    let htmlsWithLayout = map (\(a, b) -> htmlTemplate a headers $ layout a b) $ zip filenames htmls

    mapM_ (\(name, html) -> renderPage (_pageURL config </> name) config html) (zip filenames htmlsWithLayout)

    