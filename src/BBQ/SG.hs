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
import BBQ.SG.Components.Pages
import BBQ.SG.Components.Wiki
import BBQ.SG.Tools.IO
import BBQ.SG.Misc

runSG config indexLayout postsLayout tagsLayout pageLayout wikiLayout = do
    (js, css) <- getJsCSS config
    let jsRoutes  = map (_jsAbsURL config  </>) js
    let cssRoutes = map (_cssAbsURL config </>) css

    let ana = analytics $ _analyticsId config
    let headers = [ scriptList jsRoutes, cssList cssRoutes ] -- Don't use analytics

    -- Mkdir if void
    prepareFolders config

    -- Generate posts and collect meta info
    metas <- postGen headers config postsLayout

    -- Generate Homepage
    homePageGen headers indexLayout config metas

    -- Generate Tags page
    tagsGen headers config metas tagsLayout

    -- Generate other pages
    pagesGen headers config meta pageLayout

    -- Generate wiki
    wikiGen headers config wikiLayout

    -- Sync Images
    syncImages config

