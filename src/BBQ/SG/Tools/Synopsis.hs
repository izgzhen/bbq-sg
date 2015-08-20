module BBQ.SG.Tools.Synopsis (
    extract,
    Synopsis(..)
) where

import qualified Data.Map as M
import BBQ.SG.Misc

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

process lns inCode = foldr reducer (inCode, [], []) lns

reducer :: String -> (Bool, [String], [String]) -> (Bool, [String], [String])
reducer line (inCode, entries, lines) = 
    let (inCode', maybeEntry', newLine) = mapper inCode line
    in  (inCode', ifAppend maybeEntry' entries, newLine : lines)
    where
        ifAppend maybeX xs = case maybeX of
            Just x  -> [x] ++ xs
            Nothing -> xs

mapper :: Bool -> String -> (Bool, Maybe String, String)
mapper inCode ln =
    let getNextInCode x = inCode `xor` (isCodePrefix x)
        notHeaderLine ic' = (ic', Nothing, ln)
    in case destruct ln of
        Empty                   -> notHeaderLine inCode
        Single w                -> notHeaderLine (getNextInCode w)
        Multiple w trailings    -> let inCode' = getNextInCode w
            in if not inCode' then
                case M.lookup w headerToList of
                    Nothing         -> notHeaderLine inCode'
                    Just translated -> 
                        let header' = toHeader trailings
                            tag     = "<a name=\"" ++ header' ++ "\"></a>" -- In header
                            link    = "[" ++ trailings ++ "](#" ++ header' ++ ")" -- In Menu
                        in (inCode', Just (translated ++ " " ++ link), "\n" ++ w ++ tag ++ trailings)
                else (inCode', Nothing, ln)


data LineStatus = Empty
                | Single String
                | Multiple String String

destruct ln = case (words ln) of
    []   -> Empty
    w:[] -> Single w
    w:ws -> Multiple w (unwords ws)



data Synopsis = Synopsis_ {
    _prelude :: Maybe String,
    _menu    :: [String]
} deriving (Show)

extract text = (Synopsis_ prelude' menu', unlines rest')
    where
        lns = lines text
        (prelude, rest) = span (\ln -> length (words ln) < 2 || M.lookup (head $ words ln) headerToList == Nothing) lns
        prelude' = case prelude of
            []  -> Nothing
            lns -> Just $ unlines lns
        (_, menu, rest') = process rest False -- Not in code block initially
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

