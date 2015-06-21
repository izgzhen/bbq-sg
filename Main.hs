{-# LANGUAGE OverloadedStrings #-}

import IO
import Plugin
import Template
import Config
import Text.Markdown


main = withMarkdownAll $ \(text, title) -> do
        let mainHtml = markdown def text
        let headers = [ analytics "UA-54265105-1" ]
        let html = htmlTemplate title headers $ mainHtml >> mathjax
        return html
