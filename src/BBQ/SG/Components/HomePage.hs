module BBQ.SG.Components.HomePage (homePageGen) where

import BBQ.SG.Build
import BBQ.SG.Template
import BBQ.SG.Plugin
import BBQ.SG.Config

-- index is a function provided by user to generate index.html
homePageGen config metas (layout, resources) = renderPage "index" config $
    htmlTemplate "Index" (map (resourceToHeader config) resources) $ layout metas
    

