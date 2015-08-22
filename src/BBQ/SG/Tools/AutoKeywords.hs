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
import qualified Data.Set as S

unique = S.toList . S.fromList

generateBlacklist n config = do
    blacklist <- lines <$> readFile (_blacklist config)
    files <- map (_postsSrc config </>) <$> getFilesEndWith (_postsSrc config) ".md"
    contents <- mapM readFile files
    let commonWords = foldr (M.unionWith (+)) M.empty $ map (reverseMap . selectHighest n blacklist) contents
    let reallyCommonWords = deleteFindMaxN n $ reverseMap commonWords
    let blacklist' = D.union blacklist $ map snd reallyCommonWords

    return (contents, unique blacklist', files)


generateKeyWords config = do
    (contents, blacklist, files) <- generateBlacklist 30 config
    return $ zip files $ map (M.filter (>= 5) . reverseMap . selectHighest 10 blacklist) contents

