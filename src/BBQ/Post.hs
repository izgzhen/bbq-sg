module BBQ.Post (
  postTask
) where

import BBQ.Import

import Data.Time (UTCTime)
import Data.Maybe
import Control.Monad.Reader
import System.FilePath
import Data.List.Split
import Data.List
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Language.Haskell.TH
import BBQ.Task
import qualified Data.HashMap.Lazy as HM
import Text.Pandoc
import Data.Time.ISO8601
import qualified Data.Text.Lazy as T

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
                f :: Text -> Text -> Text -> PostMeta
                f text gitDate path =
                    let
                        Right pandoc@(Pandoc meta _) = readMarkdown def (T.unpack text)
                        title = writeAsciiDoc def (Pandoc nullMeta [Plain (docTitle meta)])
                        Just cDate = parseISO8601
                                   . (++"T00:00:00Z") -- Append time
                                   . intercalate "-" . take 3 . splitOn "-" -- Filename starts with 'year-month-day'
                                   . takeFileName . dropExtension
                                   $ T.unpack path
                        Just mDate = case lines (T.unpack gitDate) of
                                          [] -> return cDate -- If not checked in yet
                                          (x:_) -> parseISO8601
                                                . (\[d,t,z] -> d ++ "T" ++ t ++ z) . words -- Convert to proper ISO8601 date
                                                $ x
                        url = URL $ T.pack (dropExtension (T.unpack path))

                    in PostMeta (PostId (T.pack title) url) mDate pandoc []

        render' = WriteTask f
            where
                f hamlet (pm@PostMeta{..}, pe@PostExtra{..}) = do
                    tDir <- targetDir <$> askBuild
                    let html = $(hamletFile $(templDirQ "post.hamlet")) ()
                    let URL link = url pm
                    return (tDir </> T.unpack link ++ ".html", renderHtml html)

        -- reduce
        summarize' ps pm = ps { categories = foldl f (categories ps) (tags pm) }
            where
                f :: HashMap Text [PostId] -> Text -> HashMap Text [PostId]
                f m tag = case HM.lookup tag m of
                            Nothing   -> HM.insert tag [pid pm]   m
                            Just pids -> HM.insert tag (pid pm:pids) m

        -- map
        relate' ps pm = PostExtra { related = pids }
            where
                m = categories ps
                pids = concat $ catMaybes $ map (\tag -> HM.lookup tag m) (tags pm)

        initialSummary' = PostSummary { categories = HM.empty }

