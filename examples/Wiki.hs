module Wiki where

import qualified Text.Blaze.Html5 as H
import qualified BBQ.SG.Plugin as P
import Config
wikiLayout title list maybeBody = do
    H.h2 $ H.toHtml title
    H.section $ do
        H.h3 $ H.toHtml "[Menu]"
        P.urlList list
    case maybeBody of
        Nothing -> return ()
        Just body -> H.section $ do
                        H.h3 $ H.toHtml "[Content]"
                        H.hr
                        body

