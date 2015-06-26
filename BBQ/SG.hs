{-# LANGUAGE OverloadedStrings #-}

module BBQ.SG (
  Config(..)
, runSG
) where

import BBQ.SG.IO
import BBQ.SG.Plugin
import BBQ.SG.Template
import BBQ.SG.Meta
import BBQ.SG.Config
import Text.Markdown
import Text.Blaze.Html5 as H hiding (map)
import System.FilePath((</>))
import Text.Blaze.Html5.Attributes as A

gen headers (text, meta) = htmlTemplate title headers $ do
    H.h1 $ toHtml title
    H.h5 $ toHtml ((showMaybe $ _author meta) ++ (showMaybe $ _date meta))
    H.hr
    H.section
        mainHtml
    H.p $ do
        toHtml $ "Tags: " ++ (show $ _tags meta)
        H.br
        H.a ! A.href "../index.html"
              $ "Back to index page"
    copyRight
    mathjax

   where mainHtml = markdown def text
         title    = showMaybeStr $ _title meta

runSG config index = do
    (js, css) <- getJsCSS config
    let indexJsRoutes  = map (_jsSrcDir config </>) js
    let indexCssRoutes = map (_cssSrcDir config </>) css
    let postJsRoutes   = map (".." </>) indexJsRoutes
    let postCssRoutes  = map (".." </>) indexCssRoutes

    let ana = analytics $ _analyticsId config


    -- Generate posts
    metas <- withMarkdownAll config (gen [ana, scriptList postJsRoutes, cssList postCssRoutes])

    -- Generate Index
    withIndex config $ do
        let headers = [ ana, scriptList indexJsRoutes, cssList indexCssRoutes ]
        let mainHtml = index metas -- index is a function provided by user to generate index.html
        let html = htmlTemplate "Index" headers (mainHtml >> copyRight)
        return html

    -- Generate Tag Index
    genTagsIndex config metas $ \mainHtml -> do
        let headers = [ ana, scriptList indexJsRoutes, cssList indexCssRoutes ]
        return $ htmlTemplate "Tags" headers (mainHtml >> copyRight)

    syncImages config
