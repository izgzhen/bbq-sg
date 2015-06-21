{-# LANGUAGE OverloadedStrings #-}
import IO
import Text.Markdown
import Data.Text.Lazy (pack)
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

analytics analyticsId = H.script $ toMarkup string
  where
    string :: String
    string = 
            "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){" ++
            "(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o)," ++
            "m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)" ++ 
            "})(window,document,'script','//www.google-analytics.com/analytics.js','ga');" ++
            "ga('create', '" ++ analyticsId ++ "', 'auto'); ga('send', 'pageview');"


mathjax = do
            H.script ! A.type_ "text/javascript"
                     ! A.src   "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML" $ ""
            H.script ! A.type_ "text/x-mathjax-config" $ "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\\\(','\\\\)']]} });"
          

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

main = do
    let title = "test"
    Just text <- readMarkdownFile title
    let mainHtml = markdown def $ pack text
    let headers = [ analytics "UA-54265105-1" ]
    let html = htmlTemplate title headers $ mainHtml >> mathjax
    writeHtmlFile title (renderHtml html)
