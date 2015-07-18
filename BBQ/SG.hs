{-# LANGUAGE OverloadedStrings #-}

module BBQ.SG (
  Config(..)
, runSG
) where

import BBQ.SG.Plugin
import BBQ.SG.Template
import BBQ.SG.Meta
import BBQ.SG.Config
import Text.Blaze.Html5 as H hiding (map)
import System.FilePath((</>))
import Text.Blaze.Html5.Attributes as A
import BBQ.SG.Components.Posts
import BBQ.SG.Components.HomePage
import BBQ.SG.Components.Tags
import BBQ.SG.Tools.IO
import BBQ.SG.Misc

runSG config indexLayout postsLayout tagsLayout = do
    (js, css) <- getJsCSS config
    let indexJsRoutes  = map (_jsURL config  </>) js
    let indexCssRoutes = map (_cssURL config </>) css

    -- Currently, this level of problem can't be solved neatly
    -- Unless we can create a function *knowing what does root URL means*
    -- But it could be the next ticket
    let postJsRoutes   = map (".." </>) indexJsRoutes
    let postCssRoutes  = map (".." </>) indexCssRoutes

    let ana = analytics $ _analyticsId config
    let indexHeaders = [ ana, scriptList indexJsRoutes, cssList indexCssRoutes ]

    -- Mkdir if void
    prepareFolders config

    -- Generate posts and collect meta info
    metas <- postGen [ ana, scriptList postJsRoutes, cssList postCssRoutes ] config postsLayout

    -- Generate Homepage
    homePageGen indexHeaders indexLayout config metas

    -- Generate Tags page
    tagsGen indexHeaders config metas tagsLayout

    -- Sync Images
    syncImages config
