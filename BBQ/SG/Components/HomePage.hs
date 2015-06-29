module BBQ.SG.Components.HomePage (homePageGen) where
import BBQ.SG.Tools.IO
import BBQ.SG.Template
import BBQ.SG.Plugin

homePageGen headers layout config metas = withPage "index" config $ do
    let mainHtml = layout metas -- index is a function provided by user to generate index.html
    let html = htmlTemplate "Index" headers mainHtml
    return html

