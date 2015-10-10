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
wikiTask = Task extract summarize render relate (Just buildWidget) (Just emerge) initialSummary
    where
        extract = ReadTask f
            where
                f text gitDate path = do
                  (name, title, cDate, mDate, pandoc) <- markDownExtract text gitDate path
                  return $ WikiMeta (WikiId $ pack name) title mDate pandoc

        render = WriteTask f deps
            where
                f (wm@WikiMeta{..}, we@WikiExtra{..}) = do
                    link <- absolutePath $ Wiki wid
                    path <- filePath $ Wiki wid
                    let html = $(hamletFile $(templDirQ "wiki.hamlet")) ()
                    return (path, renderHtml html)
                deps = [$(templDirQ "wiki.hamlet")]

        summarize ws@WikiSummary{..} wm = return $ ws { subWikis = wid wm : subWikis }

        relate ws wm = return $ WikiExtra { menu = subWikis ws }

        initialSummary = WikiSummary []

        emerge :: WriteTask (WikiSummary, [FilePath], FilePath)
        emerge = WriteTask f deps
            where
                f :: (WikiSummary, [FilePath], FilePath) -> Build (FilePath, Text)
                f (ws@WikiSummary{..}, subDirs, rootDir) = do
                    -- subWikis
                    -- subDirs
                    -- rootDir
                    let name = takeFileName rootDir
                    return ("build" ++ rootDir </> name ++ ".md", "nothing here :(")

                deps = []


        buildWidget ws@WikiSummary{..} = (,) "wikiList" $ [hamlet|
                <p>This is wiki widget example
            |]
