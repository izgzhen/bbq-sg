{-# LANGUAGE OverloadedStrings #-}

module BBQ.SG.Components.Posts (postGen) where
import BBQ.SG.Tools.IO
import System.FilePath ((</>))
import BBQ.SG.Tools.Synopsis
import BBQ.SG.Template
import BBQ.SG.Plugin
import BBQ.SG.Misc
import BBQ.SG.Config
import Text.Markdown
import Data.Text.Lazy (pack)

postGen headers config layout = withMarkdownsAll config (f config headers layout)

mergeSynopsis (Synopsis_ prelude menu) = unlines $ prelude' ++ menu
    where
        prelude' = case prelude of
            Nothing -> []
            Just p  -> [p]

f config headers layout (text, meta) synopsis keywords = htmlTemplate title headers html
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
                (addTagLink config (_tags meta))
            mathjax
            showKeyWords keywords

addTagLink config tags = map (\tag -> (tag, ".." </> _tagsURL config </> toURL tag ++ ".html")) tags
