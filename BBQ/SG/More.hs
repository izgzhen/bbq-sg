module BBQ.SG.More where
import Data.Map as M hiding (foldr, map)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as A
import BBQ.SG.Plugin as P
import Text.Blaze.Html.Renderer.Text
import BBQ.SG.Template
import BBQ.SG.Config
import Data.Text.Lazy.IO (writeFile)
import Prelude hiding (writeFile)

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

genTagsIndex config metas f = do
    print "Generating tags index file..."
    -- forall a b. (a -> b -> b) -> b -> [a] -> b
    let m = foldr collectTags M.empty metas
    let infoDict = map (\tag -> let (Just metas) = M.lookup tag m
                                in (tag, map (\meta@(Meta_ (Just t) _ _ _ p) -> (t, p)) metas)
                       ) $ M.keys m

    let mainHtml = mapM_ (\(tag, list) -> H.div $ do
                        H.h4 $ toHtml tag
                        P.urlList list
                         ) infoDict
    html <- f mainHtml
    writeHtmlFile ((_staticDir config) ++ "/tags") (renderHtml html)


writeHtmlFile filename = writeFile (filename ++ ".html")