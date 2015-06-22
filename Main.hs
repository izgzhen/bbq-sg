{-# LANGUAGE OverloadedStrings #-}

import IO
import Plugin
import Template
import Config
import Text.Markdown
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A


main = do
    -- Generate posts
    withMarkdownAll $ \(text, title) -> do
        let mainHtml = markdown def text
        let headers = [ analytics analyticsId ]

        let html = htmlTemplate title headers $ do
                    mainHtml
                    H.p $ H.a ! A.href "../index.html"
                              $ "Back to index page"
                    mathjax

        return html
    -- Generate Index
    withIndex $ \pages -> do
        let mainHtml = do
            H.p "Welcome to my blog"
            H.ul $ do
                let item (name, url) = H.li $ H.a ! A.href (H.toValue url) $ H.toHtml name
                mapM_ item pages

        let headers = [ analytics analyticsId ]
        let html = htmlTemplate "Index" headers mainHtml
        return html

