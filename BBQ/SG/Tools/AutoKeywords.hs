{-
    Current Algorithms:

    User can provide a "blacklist", containing words like "the", "I", which apparently can't be the tag.
    Algorithm will also take all posts into consideration and generate a overall "common-see" list and blacklist
    them all.

-}

module BBQ.SG.Tools.AutoKeywords where
import Control.Applicative
import BBQ.SG.Config
import BBQ.SG.Tools.WordCount
import System.FilePath((</>))
import BBQ.SG.Misc (getFilesEndWith)
import qualified Data.Map as M
import qualified Data.List as D

generateKeyWords config = do
    let markdownPath = _markdownDir config
    blacklist <- lines <$> readFile (_blacklist config)
    files <- map (markdownPath </>) <$> getFilesEndWith markdownPath ".md"
    contents <- mapM readFile files
    let commonWords = foldr (M.unionWith (+)) M.empty $ map (reverseMap . selectHighest 30 blacklist) contents
    let reallyCommonWords = deleteFindMaxN 30 $ reverseMap commonWords
    let blacklist' = D.union blacklist $ map snd reallyCommonWords

    return $ zip files $ map (reverseMap . selectHighest 10 blacklist') contents


