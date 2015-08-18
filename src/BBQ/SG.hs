{-# LANGUAGE OverloadedStrings #-}

module BBQ.SG (
  Config(..)
, runSG
, BBQ.SG.Config.ResourceSpec(..)
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

runSG config indexUser postsUser tagsUser pageUser wikiUser = do
    -- Mkdir if void
    prepareFolders config

    -- Generate posts and collect meta info
    metas <- postGen config postsUser

    -- Generate Homepage
    homePageGen config metas indexUser

    -- Generate Tags page
    tagsGen config metas tagsUser

    -- Generate other pages
    pagesGen config meta pageUser

    -- Generate wiki
    wikiGen config wikiUser

    -- Sync Images
    syncImages config

