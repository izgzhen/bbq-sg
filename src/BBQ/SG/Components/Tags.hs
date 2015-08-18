module BBQ.SG.Components.Tags (tagsGen) where

import BBQ.SG.Meta
import BBQ.SG.Template
import Data.Map as M hiding (foldr, map)
import System.FilePath ((</>))
import BBQ.SG.Config
import BBQ.SG.Tools.IO
import BBQ.SG.Misc
import BBQ.SG.Plugin as P

collectTags :: Meta -> M.Map String [Meta] -> M.Map String [Meta]
collectTags meta m = foldr (f meta) m (_tags meta)
    where
        f :: Meta -> String -> M.Map String [Meta] -> M.Map String [Meta]
        f meta tag m = M.insert tag metas' m
            where
                mMetas = M.lookup tag m
                metas' = case mMetas of
                    Nothing    -> [meta]
                    Just metas -> meta : metas


tagsGen config metas (layout, resources) = 
    let m = foldr collectTags M.empty metas
        tags = M.keys m
        infoDict = map (\tag ->
                            let (Just metas) = M.lookup tag m
                            in (tag, map (\meta@(Meta_ (Just t) _ _ _ p) -> (t, ".." </> p)) metas)
                       ) tags

        genTagPage (tagName, list) = (toURL tagName, htmlTemplate
                                                     "Tag"
                                                     (map (resourceToHeader config) resources)
                                                     (pageTemplate ("Tag: " ++ tagName) (layout list)))

        htmls = map genTagPage infoDict
    in mapM_ (\(name, html) -> withPage (_tagsURL config </> name) config html) htmls
