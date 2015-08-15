module Page where

import qualified Text.Blaze.Html5 as H
import Config

pageLayout title body = do
    H.h3 $ H.toHtml title
    H.section body
    
