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
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

runSG config index = do
    -- Generate posts
    let ana = analytics $ _analyticsId config
    withMarkdownAll config $ \(text, meta) -> do
        let mainHtml = markdown def text
        let headers = [ ana ]
        let html = htmlTemplate (showMaybe $ _title meta) headers $ do
                    mainHtml
                    H.p $ H.a ! A.href "../index.html"
                              $ "Back to index page"
                    mathjax

        return html
    -- Generate Index
    withIndex config $ \pages -> do
        let mainHtml = index pages
        let headers = [ ana ]
        let html = htmlTemplate "Index" headers mainHtml
        return html

showMaybe Nothing  = ""
showMaybe (Just a) = show a