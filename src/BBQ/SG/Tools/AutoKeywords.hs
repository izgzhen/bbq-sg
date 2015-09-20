module BBQ.SG.Tools.AutoKeywords (
  generateKeyWords
) where

import Control.Applicative
import BBQ.SG.Config
import BBQ.SG.Tools.WordCount
import System.FilePath((</>))
import qualified Data.Map as M
import qualified Data.List as D
import qualified Data.Set as S

-- generateKeyWords config filenames = do
--     blacklist <- lines <$> readFile (_blacklist config)
--     let files = map (\f -> _postsSrc config </> f ++ ".md") filenames
--     contents <- mapM readFile files
--     let commonWords = map (selectHighest 10 blacklist) contents
--     return $ zip filenames $ map (M.filter (>= 5) . reverseMap) commonWords

generateKeyWords = undefined