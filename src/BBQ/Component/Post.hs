module BBQ.Component.Post (
  postTask
, PostWidget(..)
) where

import BBQ.Import
import BBQ.Task
import qualified Data.HashMap.Lazy as HM
import BBQ.Component.Common
import qualified Data.Set as S

data PostMeta = PostMeta {
  pid     :: PostId
, title   :: Text
, date    :: UTCTime
, body    :: Pandoc
, tags    :: [Text]
}

data PostSummary = PostSummary {
  categories :: HashMap Text [PostId] -- Tag -> Posts
}

data PostWidget = PostWidget {
  allPosts   :: S.Set PostId
} deriving (Generic, Show)

instance ToJSON PostWidget
instance FromJSON PostWidget


postTask :: Task PostMeta PostSummary
postTask = Task "md" extract summarize renderIndex renderPage renderWidget
    where
        extract filepath gitDate text =
            case markDownExtract text gitDate filepath of
                Nothing -> Nothing
                Just (name, title, _, mDate, pandoc) ->
                    Just $ PostMeta (PostId $ pack name) title mDate pandoc []

        summarize _ _ metaMap = PostSummary $ HM.foldr f HM.empty metaMap
            where
                f :: PostMeta -> HashMap Text [PostId] -> HashMap Text [PostId]
                f PostMeta{..} m = 
                    foldr (\tag m -> case HM.lookup tag m of
                                        Nothing   -> HM.insert tag [pid] m
                                        Just pids -> HM.insert tag (pid : pids) m)
                          m tags

        renderIndex PostSummary{..} = do
            need [$(templDirQ "index.hamlet")]
            let html = $(hamletFile $(templDirQ "index.hamlet")) ()
            return $ renderHtml html

        renderPage PostSummary{..} PostMeta{..} = do
            need [$(templDirQ "post.hamlet")]
            let html = $(hamletFile $(templDirQ "post.hamlet")) ()
            return $ renderHtml html

        renderWidget PostSummary{..} = do
            let widget = PostWidget . S.fromList . concat $ HM.elems categories
            return $ encode widget