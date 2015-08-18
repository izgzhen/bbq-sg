{-# LANGUAGE OverloadedStrings #-}

module BBQ.SG.Plugin (
  analytics
, mathjax
, urlList
, BBQ.SG.Plugin.p
, BBQ.SG.Plugin.a
, BBQ.SG.Plugin.h3
, BBQ.SG.Plugin.h5
, showMaybe
, showMaybeStr
, copyRight
, getToday
, Meta(..)
, scriptify
, cssify
, showKeyWords
) where

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5((!))
import qualified Text.Blaze.Html5.Attributes as A
import BBQ.SG.Meta
import Data.Time.Clock
import Data.Time.Calendar
import BBQ.SG.Misc
import qualified Data.Map as M


analytics :: String -> H.Html
analytics analyticsId = H.script $ H.toMarkup string
  where
    string :: String
    string = 
            "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){" ++
            "(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o)," ++
            "m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)" ++ 
            "})(window,document,'script','//www.google-analytics.com/analytics.js','ga');" ++
            "ga('create', '" ++ analyticsId ++ "', 'auto'); ga('send', 'pageview');"


mathjax :: H.Html
mathjax = do
            H.script ! A.type_ "text/javascript"
                     ! A.src   "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML" $ ""
            H.script ! A.type_ "text/x-mathjax-config" $ "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\\\(','\\\\)']]} });"


urlList :: [(String, FilePath)] -> H.Html
urlList list = H.ul $ do
                let itemize (name, url) = H.li $ H.a ! A.href (H.toValue url) $ H.toHtml name
                mapM_ itemize list

-- Re-export part of HTML tags
p :: H.ToMarkup a => a -> H.Html
p = H.p . H.toHtml

a :: (H.ToMarkup a, H.ToValue b) => a -> b -> H.Html
a text addr = H.a ! A.href (H.toValue addr)
                  $ H.toHtml text


h3 :: H.ToMarkup a => a -> H.Html
h3 = H.h3 . H.toHtml

h5 :: H.ToMarkup a => a -> H.Html
h5 = H.h5 . H.toHtml

copyRight :: String -> String -> H.Html
copyRight author year =
  H.div ! A.id "copyright" $ do
    H.p $ H.toHtml $ "Copyright Reserved, " ++ author ++ ", " ++ year
    H.p $ "Generated with BBQ Static Generator"

getToday = do
  (y, m, d) <- getCurrentTime >>= return . toGregorian . utctDay
  return $ show y ++ "." ++ show m ++ "." ++ show d



scriptify :: FilePath -> H.Html
scriptify path = H.script ! A.type_ "text/javascript"
                          ! A.src   (H.toValue path)
                          $ ""

cssify :: FilePath -> H.Html
cssify path = H.link ! A.href  (H.toValue path)
                     ! A.rel   "stylesheet"
                     ! A.type_ "text/css"


showKeyWords :: M.Map String Int -> H.Html
showKeyWords keywords = H.p $ H.toHtml $ show keywords
