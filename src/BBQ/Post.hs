module BBQ.Post (
  postTask
) where

import BBQ.Import

import Data.Time (UTCTime)
import Data.List.Split
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Language.Haskell.TH
import BBQ.Task
import qualified Data.HashMap.Lazy as HM
import Text.Pandoc
import Data.Time.ISO8601
import qualified Data.Text.Lazy as TL

data PostMeta = PostMeta {
  pid     :: PostId
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

data PostId = PostId Text URL

url :: PostMeta -> URL
url pm = let (PostId _ link) = pid pm in link

postTask = Task extract' summarize' render' relate' initialSummary'
    where
        extract' = ReadTask f
            where
                f :: Text -> Text -> FilePath -> Build PostMeta
                f text gitDate path = do
                    let ret = do
                          pandoc@(Pandoc meta _) <- eitherToMaybe $ readMarkdown def (unpack text)
                          let title = pandocMetaToTitle meta
                          cDate <- parseCreatedDate . takeFileName $ dropExtension path
                          mDate <- parseModifiedDate gitDate cDate
                          let url = URL $ dropExtension path
                          return $ PostMeta (PostId title url) mDate pandoc []
                    case ret of
                        Nothing -> throwError "parsing failed"
                        Just pm -> return pm

                parseCreatedDate x = parseISO8601 . (++"T00:00:00Z") -- Append time
                                                  . intercalate "-" . take 3 $ splitOn "-" x
                                                 -- Filename starts with 'year-month-day'
                parseModifiedDate gitDate cDate =
                    case lines (unpack gitDate) of
                       []    -> return cDate -- If not checked in yet
                       (x:_) -> parseISO8601
                                . (\[d,t,z] -> d ++ "T" ++ t ++ z)
                                $ words x
                             -- Convert to proper ISO8601 date

                pandocMetaToTitle meta = pack $ writeAsciiDoc def (Pandoc nullMeta [Plain (docTitle meta)])

        render' = WriteTask f deps
            where
                f hamlet (pm@PostMeta{..}, pe@PostExtra{..}) = do
                    let PostId title (URL link) = pid
                    tDir <- targetDir <$> askBuild
                    let html = $(hamletFile $(templDirQ "post.hamlet")) ()
                    return (tDir </> link ++ ".html", TL.toStrict $ renderHtml html)
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



