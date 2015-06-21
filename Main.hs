{-# LANGUAGE OverloadedStrings #-}

import IO
import Plugin
import Template
import Text.Markdown

main = withFileName "test" $ \text -> do
        let mainHtml = markdown def text
        let headers = [ analytics "UA-54265105-1" ]
        let html = htmlTemplate "test" headers $ mainHtml >> mathjax
        return html
