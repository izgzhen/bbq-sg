-- Word Counter
module BBQ.SG.Tools.WordCount (
  selectHighest
) where

import qualified Data.Map as M
import Data.List.Split (splitWhen)
import Data.List (sortBy)

splitText = filter (/= "") . splitWhen (`elem` ",.\n ")

-- foldr :: forall a b. (a -> b -> b) -> b -> [a] -> b
count :: String -> M.Map String Int
count text = foldr f M.empty $ splitText text
    where
        f :: String -> M.Map String Int -> M.Map String Int
        f w m = case M.lookup w m of
            Just i  -> M.insert w (i + 1) m
            Nothing -> M.insert w 1       m

deleteFindMaxN :: Int -> M.Map k v -> [(k, v)]
deleteFindMaxN n m = if n == 0 || M.size m == 0 then [] else
    let (max, m') = M.deleteFindMax m
    in  max : deleteFindMaxN (n - 1) m'

selectHighest :: Int -> [String] -> String -> M.Map Int String
selectHighest i blacklist text = M.fromList highests
    where
        m   = count text
        m'  = clean m blacklist
        m'' = reverseMap m'
        highests = deleteFindMaxN i m''

reverseMap :: Ord v => M.Map k v -> M.Map v k
reverseMap = M.fromList . map (\(k, v) -> (v, k)) . M.toList

clean m blacklist = case blacklist of
    []   -> m
    b:bs -> clean (M.delete b m) bs
