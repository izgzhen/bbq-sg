{-# LANGUAGE OverloadedStrings #-}

module BBQ.SG.Plugin (
  analytics
, mathjax
, urlList
, BBQ.SG.Plugin.p
, _title
, showMaybe
, showMaybeStr
, copyRight
) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import BBQ.SG.Meta

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


urlList list = H.ul $ do
                let itemize (name, url) = H.li $ H.a ! A.href (H.toValue url) $ H.toHtml name
                mapM_ itemize list

-- Re-export part of HTML tags
p :: ToMarkup a => a -> Html
p = H.p . toHtml


copyRight = H.div $ H.p "Copyright Reserved, Zhen Zhang, 2015"