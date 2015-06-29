module BBQ.SG.Tools.Synopsis where

import qualified Data.Map as M

headerToList = M.fromList [
      ("#", "1.")
    , ("##", "\t*")
    , ("###", "\t\t+")
    , ("####", "\t\t\t-")
      ]

isCodePrefix s = length s >= 3 && take 3 s == "```"

xor True True   = False
xor False False = False
xor True False  = True
xor False True  = True

process []       inCode = []
process (ln:lns) inCode = case eEntry of
    Left iC'       -> process lns iC'
    Right (iC', e) -> e : process lns iC'
    where
        wds = words ln
        e   = case wds of
            []   -> Left inCode
            w:[] -> Left $ inCode `xor` isCodePrefix w
            w:ws -> Right (w, unwords ws)

        eEntry = do
            (prefix, header) <- e
            let inCode' = inCode `xor` isCodePrefix prefix

            if not inCode' then case M.lookup prefix headerToList of
                Nothing         -> Left inCode'
                Just translated -> Right (inCode', translated ++ " " ++ header)
            else Left inCode'


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
        menu  = process rest False -- Not in code block initially
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

