{-# LANGUAGE OverloadedStrings #-}

module BBQ.SG.Plugin (
  analytics
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


copyRight :: String -> String -> H.Html
copyRight author year =
  H.div $ do
    H.p $ H.toHtml $ "Copyright Reserved, " ++ author ++ ", " ++ year
    H.p $ "Generated with BBQ Static Generator"

getToday = do
  (y, m, d) <- getCurrentTime >>= return . toGregorian . utctDay
  return $ show y ++ "." ++ show m ++ "." ++ show d



scriptList :: [FilePath] -> H.Html
scriptList scripts = mapM_ (\s -> H.script ! A.type_ "text/javascript"
                                           ! A.src   (H.toValue s)
                                           $ ""
                           ) scripts

cssList :: [FilePath] -> H.Html
cssList csses = mapM_ (\c -> H.link ! A.href  (H.toValue c)
                                    ! A.rel   "stylesheet"
                                    ! A.type_ "text/css"
                      ) csses


showKeyWords :: M.Map String Int -> H.Html
showKeyWords keywords = H.p $ H.toHtml $ show keywords
