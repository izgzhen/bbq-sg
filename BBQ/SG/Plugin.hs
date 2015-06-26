{-# LANGUAGE OverloadedStrings #-}

module BBQ.SG.Plugin (
  Plugin
, Snippet
, analytics
, mathjax
, urlList
, BBQ.SG.Plugin.p
, showMaybe
, showMaybeStr
, copyRight
, getToday
, BBQ.SG.Plugin.a
, Meta(..)
, scriptList
, cssList
) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import BBQ.SG.Meta
import Data.Time.Clock
import Data.Time.Calendar
import BBQ.SG.Misc


type Plugin  a = a -> Html
type Snippet   = Html


analytics :: Plugin String
analytics analyticsId = H.script $ toMarkup string
  where
    string :: String
    string = 
            "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){" ++
            "(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o)," ++
            "m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)" ++ 
            "})(window,document,'script','//www.google-analytics.com/analytics.js','ga');" ++
            "ga('create', '" ++ analyticsId ++ "', 'auto'); ga('send', 'pageview');"

mathjax :: Snippet
mathjax = do
            H.script ! A.type_ "text/javascript"
                     ! A.src   "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML" $ ""
            H.script ! A.type_ "text/x-mathjax-config" $ "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\\\(','\\\\)']]} });"


urlList :: Plugin [(String, FilePath)]
urlList list = H.ul $ do
                let itemize (name, url) = H.li $ H.a ! A.href (H.toValue url) $ H.toHtml name
                mapM_ itemize list

-- Re-export part of HTML tags
p :: ToMarkup a => a -> Html
p = H.p . toHtml

a :: (ToMarkup a, ToValue b) => a -> b -> Html
a text addr = H.a ! A.href (toValue addr)
                  $ toHtml text


copyRight :: Snippet
copyRight = H.div $ H.p "Copyright Reserved, Zhen Zhang, 2015"

getToday = do
  (y, m, d) <- getCurrentTime >>= return . toGregorian . utctDay
  return $ show y ++ "." ++ show m ++ "." ++ show d



scriptList :: Plugin [FilePath]
scriptList scripts = mapM_ (\s -> H.script ! A.type_ "text/javascript"
                                           ! A.src   (toValue s)
                                           $ ""
                           ) scripts

cssList :: Plugin [FilePath]
cssList csses = mapM_ (\c -> H.link ! A.href  (toValue c)
                                    ! A.rel   "stylesheet"
                                    ! A.type_ "text/css"
                      ) csses

