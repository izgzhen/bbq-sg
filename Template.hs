{-# LANGUAGE OverloadedStrings #-}

module Template (
    htmlTemplate
) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A


htmlTemplate :: String -> [H.Html] -> H.Html -> H.Html
htmlTemplate title headers body =
    H.html $ do
        H.head $ do
            H.title (H.toHtml title)
            H.meta ! A.httpEquiv "Content-Type"
                   ! A.content "text/html;charset=utf-8"
            sequence_ headers
        H.body $ do
            body
