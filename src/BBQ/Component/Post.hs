module BBQ.Component.Post (
  postTask
, PostMeta(..)
, PostSummary(..)
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
, date    :: Maybe UTCTime
, body    :: Pandoc
, tags    :: [Text]
}

data PostSummary = PostSummary {
  categories :: HashMap Text [PostId],
  postList   :: [PostId]
}

data PostWidget = PostWidget {
  allPosts   :: S.Set PostId
} deriving (Generic, Show)

instance ToJSON PostWidget
instance FromJSON PostWidget


postTask :: Task PostMeta PostSummary
postTask = Task "md" extract summarize renderWidget
    where
        extract filepath gitDate text =
            case markDownExtract text gitDate filepath of
                Nothing -> Nothing
                Just (name, title, mDate, pandoc) ->
                    Just $ PostMeta (PostId $ pack name) title mDate pandoc []

        summarize _ _ metaMap = uncurry PostSummary $ HM.foldr f (HM.empty, []) metaMap
            where
                f PostMeta{..} (m, l) = (m', l')
                  where
                    m' = foldr (\tag m -> case HM.lookup tag m of
                                        Nothing   -> HM.insert tag [pid] m
                                        Just pids -> HM.insert tag (pid : pids) m)
                               m tags
                    l' = l ++ [pid]

        renderWidget PostSummary{..} = do
            let widget = PostWidget $ S.fromList postList
            return $ encode widget
            