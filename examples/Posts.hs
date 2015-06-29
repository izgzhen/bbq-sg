module Posts where

import BBQ.SG.Plugin
import qualified Text.Blaze.Html5 as H

postsLayout :: H.Html -> H.Html -> String -> String -> [String] -> H.Html
postsLayout synopsis mainHtml author date tags = do
    H.h5 $ H.toHtml "Synopsis"
    H.section $ synopsis
    H.h5 $ H.toHtml (author ++ " " ++ date)
    H.hr
    H.section $ mainHtml
    H.section $ H.toHtml $ "Tags: " ++ listify tags


listify []     = ""
listify (w:[]) = w
listify (w:ws) = w ++ ", " ++ listify ws
