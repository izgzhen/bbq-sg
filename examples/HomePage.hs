module HomePage where

import qualified Text.Blaze.Html5 as H
import qualified BBQ.SG.Plugin as P
import BBQ.SG.Plugin
import Config
import Data.List (sortBy)

homeLayout metas = do
    H.section $ do
        P.p "Hello, welcome to my blog"

    H.section $ do
        let metas' = sortBy (\ma mb -> compare (_date mb) (_date ma)) metas
        let posts = map (\meta -> (P.showMaybeStr $ _title meta, _path meta)) metas'
        P.h3 "Post list"
        P.urlList posts

