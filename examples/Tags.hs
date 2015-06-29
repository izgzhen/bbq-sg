module Tags where

import qualified BBQ.SG.Plugin as P
import qualified Text.Blaze.Html5 as H

tagsLayout infoDict =  mapM_ (\(tag, list) -> H.div $ do
        H.h4 $ H.toHtml tag
        P.urlList list
    ) infoDict
