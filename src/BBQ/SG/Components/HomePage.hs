module BBQ.SG.Components.HomePage (homePageGen) where
import BBQ.SG.Tools.IO
import BBQ.SG.Template
import BBQ.SG.Plugin
import BBQ.SG.Config

homePageGen config metas (layout, resources) = renderPage "index" config $
    -- index is a function provided by user to generate index.html
    htmlTemplate "Index" (map (resourceToHeader config) resources) $ layout metas
    

