{-# LANGUAGE OverloadedStrings #-}

module BBQ.SG.Components.Posts (postGen) where
import BBQ.SG.Tools.IO
import BBQ.SG.Tools.Synopsis
import BBQ.SG.Template
import BBQ.SG.Plugin
import Text.Markdown
import Data.Text.Lazy (pack)

postGen headers config layout = withMarkdownsAll config (f headers layout)

mergeSynopsis (Synopsis_ prelude menu) = unlines $ prelude' ++ menu
    where
        prelude' = case prelude of
            Nothing -> []
            Just p  -> [p]

f headers layout (text, meta) synopsis keywords = htmlTemplate title headers html
    where
        synopsisHtml = markdown def (pack $ mergeSynopsis synopsis)
        mainHtml = markdown def text
        title    = showMaybeStr $ _title meta
        html     = pageTemplate title $ do
            layout
                synopsisHtml
                mainHtml
                (showMaybe $ _author meta)
                (showMaybe $ _date meta)
                (_tags meta)
            mathjax
            showKeyWords keywords

