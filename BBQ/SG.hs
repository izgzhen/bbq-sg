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

gen ana (text, meta) = htmlTemplate title headers $ do
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
         headers  = [ ana ]

runSG config index = do
    let ana = analytics $ _analyticsId config

    -- Generate posts
    metaDict <- withMarkdownAll config (gen ana)

    -- Generate Index
    withIndex config $ do
        let mainHtml = index metaDict -- index is a function provided by user to generate index.html
        let headers = [ ana ]
        let html = htmlTemplate "Index" headers (mainHtml >> copyRight)
        return html

    syncImages config
