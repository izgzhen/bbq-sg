module BBQ.Component.Wiki (
  wikiTask
, WikiMeta(..)
, WikiSummary(..)
, WikiWidget(..)
) where

import BBQ.Import
import BBQ.Task
import BBQ.Component.Common
import qualified Data.HashMap.Lazy as HM
-- import BBQ.Route

data WikiMeta = WikiMeta {
  wid     :: WikiId
, title   :: Text
, date    :: UTCTime
, body    :: Pandoc
}

data WikiSummary = WikiSummary {
  subWikis :: [FilePath] -- SubWiki URLs relative to root
}

data WikiWidget = WikiWidget {
  wikis :: [FilePath]
} deriving (Generic, Show)

instance ToJSON WikiWidget
instance FromJSON WikiWidget

wikiTask :: Task WikiMeta WikiSummary
wikiTask = Task "md" extract summarize renderWidget
    where
        extract filepath gitDate text =
            case markDownExtract text gitDate filepath of
                Nothing -> Nothing
                Just (name, title, _, mDate, pandoc) ->
                    Just $ WikiMeta (WikiId $ pack name) title mDate pandoc

        summarize parentDir subDirs metaMap =
            let fileNames = map (parentDir </>) $ HM.keys metaMap
            in  WikiSummary (fileNames ++ subDirs)

        -- renderIndex WikiSummary{..} = do
        --     need [$(templDirQ "wiki-index.hamlet")]
        --     let html = $(hamletFile $(templDirQ "wiki-index.hamlet")) ()
        --     return $ renderHtml html

        -- renderPage WikiSummary{..} WikiMeta{..} = do
        --     need [$(templDirQ "wiki.hamlet")]
        --     let html = $(hamletFile $(templDirQ "wiki.hamlet")) ()
        --     return $ renderHtml html

        renderWidget WikiSummary{..} = do
            let widget = WikiWidget subWikis
            return $ encode widget
