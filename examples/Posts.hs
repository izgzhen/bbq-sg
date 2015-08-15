module Posts where

import qualified BBQ.SG.Plugin as P
import qualified Text.Blaze.Html5 as H
import Config

-- postsLayout :: H.Html -> H.Html -> String -> String -> [(String, FilePath)] -> H.Html
postsLayout synopsis mainHtml author date tagsMap keyWords = do
    P.h5 $ H.toHtml (author ++ " " ++ date)
    P.h3 $ H.toHtml "[Synopsis]"
    H.section $ synopsis
    goBackFromPost
    H.hr

    H.section $ do
        P.h3 "[Content]"
        mainHtml

    H.section $ do
        P.h3 "[Meta information]"
        P.h5 "Tags:"
        P.urlList tagsMap
        P.p $ "Keywords (Unstable): " ++ listify (map fst keyWords)

    myCopyRight

listify []     = ""
listify (w:[]) = w
listify (w:ws) = w ++ ", " ++ listify ws
