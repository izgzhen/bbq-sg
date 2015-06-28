module BBQ.SG.Tools.Synopsis where

import qualified Data.Map as M

headerToList = M.fromList [
      ("#", "1.")
    , ("##", "\t*")
    , ("###", "\t\t+")
    , ("####", "\t\t\t-")
      ]


process []       = []
process (ln:lns) = entry ++ process lns
    where
        wds = words ln
        m   = if length wds > 1 then Just (head wds, unwords $ tail wds)
                                else Nothing
        mEntry = do
            (prefix, header) <- m
            translated <- M.lookup prefix headerToList
            return $ translated ++ " " ++ header

        entry = case mEntry of
            Nothing -> []
            Just ln -> [ln]

data Synopsis = Synopsis_ {
    _prelude :: Maybe String,
    _menu    :: [String]
} deriving (Show)

extract text = Synopsis_ prelude' menu'
    where
        lns = lines text
        (prelude, rest) = span (\ln -> length (words ln) < 2 || M.lookup (head $ words ln) headerToList == Nothing) lns
        prelude' = case prelude of
            []  -> Nothing
            lns -> Just $ unlines lns
        menu  = process rest
        menu' = deleteGarbageIndent menu

deleteGarbageIndent :: [String] -> [String]
deleteGarbageIndent []  = []
deleteGarbageIndent lns = let minIndent = getMinIndent lns
                          in  map (deleteIndent minIndent) lns
    where
        getMinIndent (ln:[])  = length . fst $ getIndent ln
        getMinIndent (ln:lns) = let (i, _)  = getIndent ln
                                    i'      = getMinIndent lns
                                in length i `min` i'

        getIndent ln = span (== '\t') ln

        deleteIndent indent line = let (i, rest) = getIndent line
                                       i' = take (length i - indent) $ repeat '\t'
                                   in  i' ++ rest



