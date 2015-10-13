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
, date    :: Maybe UTCTime
, body    :: Pandoc
}

data WikiSummary = WikiSummary {
  subWikis :: [WikiId]
}

data WikiWidget = WikiWidget {
  wikis :: [WikiId]
} deriving (Generic, Show)

instance ToJSON WikiWidget
instance FromJSON WikiWidget

wikiTask :: Task WikiMeta WikiSummary
wikiTask = Task "md" extract summarize renderWidget
    where
        extract filepath gitDate text =
            case markDownExtract text gitDate filepath of
                Nothing -> Nothing
                Just (_, title, mDate, pandoc) ->
                    Just $ WikiMeta (WikiId $ pack $ dropDirectory1 filepath) title mDate pandoc

        summarize parentDir subDirs metaMap =
            let fileNames = map (parentDir </>) $ HM.keys metaMap
            in  WikiSummary $ map (WikiId . pack . dropDirectory1) (fileNames ++ subDirs)

        renderWidget WikiSummary{..} = do
            let widget = WikiWidget subWikis
            return $ encode widget
