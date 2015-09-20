{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module BBQ.SG (
  Config(..)
, runSG
, BBQ.SG.Config.ResourceSpec(..)
, DevMode(..)
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
import BBQ.SG.Build
import BBQ.SG.Tools.AutoKeywords
import BBQ.SG.Misc
import Control.Monad

runSG = undefined

-- runSG config indexUser postsUser tagsUser pageUser wikiUser = do
--     -- Mkdir if void
--     prepareFolders config
--     when ((_devmode config) == Production) (writeFile (_modCache config) "") -- Flush cache when generate production site
--     resolveDeps config
--     maybeCache <- loadCache (_modCache config)
--     case maybeCache of
--         Nothing -> error "Loading cache error"
--         Just cache -> do
--             -- Generate posts and collect meta info
--             (metas, cache') <- postGen config cache postsUser

--             -- Generate Homepage
--             homePageGen config metas indexUser

--             -- Generate Tags page
--             tagsGen config metas tagsUser

--             -- Generate other pages
--             pagesGen config meta pageUser

--             -- Generate wiki
--             wikiGen config wikiUser

--             -- Sync Resources
--             cache'' <- syncImages config cache'
--                            >>= syncJs config
--                            >>= syncCss config

--             storeCache (_modCache config) cache''
            

--             putStrLn "\n>>>>>>>> AdditionalInfo <<<<<<<<"
--             case _devmode config of
--                 Preview     -> putStrLn "YOU ARE IN **PREVIEW** MODE!"
--                 Production  -> putStrLn "Production code is generated"
--                 Debug       -> putStrLn "now is DEBUG MODE"
