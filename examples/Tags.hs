module Tags where

import qualified BBQ.SG.Plugin as P
import qualified Text.Blaze.Html5 as H
import Config

tagsLayout list = do
    P.urlList list
    myCopyRight
