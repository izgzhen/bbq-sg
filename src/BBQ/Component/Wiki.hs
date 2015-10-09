module BBQ.Component.Wiki where

import BBQ.Import
import BBQ.Task
import BBQ.Component.Common

data WikiMeta = WikiMeta {
  wid     :: WikiId
, title   :: Text
, date    :: UTCTime
, body    :: Pandoc
}

data WikiSummary = WikiSummary {
  subWikis :: [FilePath]
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

        render' = undefined
        summarize' = undefined
        relate' = undefined
        buildWidget' = undefined
        initialSummary' = WikiSummary []

wikiCollectTask :: WriteTask (HashMap Text Widget)
wikiCollectTask = WriteTask f deps
    where
        f = undefined
        deps = undefined

wikiMkDirWidget :: [FilePath] -> Maybe (Text, Widget)
wikiMkDirWidget dirs = Just undefined