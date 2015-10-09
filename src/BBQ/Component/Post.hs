module BBQ.Component.Post (
  postTask
) where

import BBQ.Import

import Data.Time (UTCTime)
import Language.Haskell.TH
import BBQ.Task
import qualified Data.HashMap.Lazy as HM
import BBQ.Component.Common

data PostMeta = PostMeta {
  pid     :: PostId
, title   :: Text
, date    :: UTCTime
, body    :: Pandoc
, tags    :: [Text]
}

data PostExtra = PostExtra {
  related :: [PostId]
}

data PostSummary = PostSummary {
  categories :: HashMap Text [PostId] -- Tag -> Posts
}

postTask = Task extract' summarize' render' relate' (Just buildWidget') initialSummary'
    where
        extract' = ReadTask f
            where
              f text gitDate path = do
                (name, title, cDate, mDate, pandoc) <- markDownExtract text gitDate path
                return $ PostMeta (PostId $ pack name) title mDate pandoc []

        render' = WriteTask f deps
            where
                f (pm@PostMeta{..}, pe@PostExtra{..}) = do
                    link <- absolutePath $ Post pid
                    path <- filePath $ Post pid
                    let html = $(hamletFile $(templDirQ "post.hamlet")) ()
                    return (path, renderHtml html)
                deps = [$(templDirQ "post.hamlet")]

        -- reduce
        summarize' ps pm = return $ ps { categories = foldl' f (categories ps) (tags pm) }
            where
                f :: HashMap Text [PostId] -> Text -> HashMap Text [PostId]
                f m tag = case HM.lookup tag m of
                            Nothing   -> HM.insert tag [pid pm]   m
                            Just pids -> HM.insert tag (pid pm:pids) m

        -- map
        relate' ps pm = return $ PostExtra { related = pids }
            where
                m = categories ps
                pids = concat $ catMaybes $ map (\tag -> HM.lookup tag m) (tags pm)

        initialSummary' = PostSummary { categories = HM.empty }

        buildWidget' ps@PostSummary{..} = (,) "postsList"
                                          $ [hamlet|
                                              <p> Sorry, not developed yet :(
                                            |]

