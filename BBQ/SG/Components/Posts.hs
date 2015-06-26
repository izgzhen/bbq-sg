{-# LANGUAGE OverloadedStrings #-}

module BBQ.SG.Components.Posts (postGen) where
import BBQ.SG.Tools.IO
import BBQ.SG.Template
import BBQ.SG.Plugin
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Markdown

postGen headers config = withMarkdownsAll config $ f headers

f headers (text, meta) = htmlTemplate title headers html
    where
        mainHtml = markdown def text
        title    = showMaybeStr $ _title meta
        html     = do
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

