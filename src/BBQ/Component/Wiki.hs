module BBQ.Component.Wiki where

import BBQ.Import
import BBQ.Task
import BBQ.Component.Common
import qualified Data.HashMap.Lazy as HM
import BBQ.Route

data WikiMeta = WikiMeta {
  wid     :: WikiId
, title   :: Text
, date    :: UTCTime
, body    :: Pandoc
}

data WikiSummary = WikiSummary {
  subWikis :: [WikiId]
}

data WikiExtra = WikiExtra {
  menu :: [WikiId]
}

wikiTask :: Task WikiMeta WikiSummary WikiExtra Widget
wikiTask = Task extract' summarize' render' relate' (Just buildWidget') initialSummary'
    where
        extract' = ReadTask f
            where
                f text gitDate path = do
                  (name, title, cDate, mDate, pandoc) <- markDownExtract text gitDate path
                  return $ WikiMeta (WikiId $ pack name) title mDate pandoc

        render' = WriteTask f deps
            where
                f (wm@WikiMeta{..}, we@WikiExtra{..}) = do
                    link <- absolutePath $ Wiki wid
                    path <- filePath $ Wiki wid
                    let html = $(hamletFile $(templDirQ "wiki.hamlet")) ()
                    return (path, renderHtml html)
                deps = [$(templDirQ "wiki.hamlet")]

        summarize' ws@WikiSummary{..} wm = return $ ws { subWikis = wid wm : subWikis }

        relate' ws wm = return $ WikiExtra { menu = subWikis ws }

        initialSummary' = WikiSummary []

        buildWidget' ws@WikiSummary{..} = (,) "wikiList" $ [hamlet|
                <p>This is wiki widget example
            |]

wikiCollectTask :: FilePath -> WriteTask (HashMap Text Widget)
wikiCollectTask fp = WriteTask f deps
    where
        f widgets = do
            let mWiki = HM.lookup "wikiList" widgets
            let mWIndex = HM.lookup "wikiDirWidget" widgets
            let html = $(hamletFile $(templDirQ "wiki-index.hamlet")) contentRender
            return (fp, renderHtml html)

        deps = [$(templDirQ "wiki-index.hamlet")]

wikiMkDirWidget :: [FilePath] -> Maybe (Text, Widget)
wikiMkDirWidget dirs = Just $ (,) "wikiDirWidget" [hamlet|
        Wiki Dir Widget Examples
    |]
