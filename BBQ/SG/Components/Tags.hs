module BBQ.SG.Components.Tags (tagsGen) where
import BBQ.SG.Meta
import BBQ.SG.Template
import Data.Map as M hiding (foldr, map)
import BBQ.SG.Tools.IO
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


tagsGen headers config metas layout = withPage "tags" config $ do
    
    let m = foldr collectTags M.empty metas
    let infoDict = map (\tag -> let (Just metas) = M.lookup tag m
                                in (tag, map (\meta@(Meta_ (Just t) _ _ _ p) -> (t, p)) metas)
                       ) $ M.keys m

    let mainHtml = pageTemplate "tags" $ layout infoDict

    let html = htmlTemplate "Index" headers mainHtml

    return html
