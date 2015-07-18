module BBQ.SG.Misc where

import Control.Applicative((<$>))
import Control.Monad
import System.Directory
import System.FilePath (FilePath(..), (</>), takeExtensions, dropExtensions)
import System.Posix
import Data.List.Extra (splitOn)
import Data.List (group, sort)
import Network.URL

type EitherS = Either String -- Left is the error info

unique :: (Eq a, Ord a) => [a] -> [a]
unique = map (\(x:_) -> x) . group . sort

toURL :: String -> String
toURL filename = let Just url = importURL filename in exportURL url

fromURL :: String -> String
fromURL url = let Just (URL _ str _) = importURL url in str

joinWords :: String -> [String] -> String
joinWords _       []     = ""
joinWords spliter (w:[]) = w
joinWords spliter (w:ws) = w ++ spliter ++ joinWords spliter ws

toHeader :: String -> String
toHeader str = let url = toURL str
                   noWhiteSpace = splitOn "%20" url
               in  joinWords "-" noWhiteSpace


front = reverse . tail . reverse

clean x xs = let xs' = if head xs == x then tail xs else xs
             in if last xs' == x then front xs' else xs'

showMaybe Nothing   = ""
showMaybe (Just a) = show a


showMaybeStr Nothing  = ""
showMaybeStr (Just s) = s

-- example: "./static/img" -> "./img" with "./static" to drop
dropFirstDir prefix str   = "." ++ (head . tail $ splitOn prefix str)


getFilesEndWith path ext = do
    names <- getDirectoryContents path
    return $ filter (\name -> takeExtensions name == ext) names

readFileMaybe path = do
    exist <- doesFileExist path
    if exist then do
        text <- readFile path
        return $ Right text
        else return $ Left ("Reading " ++ show path ++ " failed")

getFileSize path = getFileStatus path >>= \s -> return $ fileSize s

getSubContents path = filter (\n -> n /= "." && n /= "..") <$> getDirectoryContents path

getFileDict path = do
    contents <- getSubContents path
    let contentsDict = zip (map (path </>) contents) contents
    dirs     <- filterM doesDirectoryExist $ map fst contentsDict
    files    <- filterM (doesFileExist . fst) contentsDict
    dict     <- concat <$> mapM getFileDict dirs
    return $ dict ++ files


splitOnPath = filter (/= "") . splitOn "/"

concatPath []     = ""
concatPath (x:[]) = x
concatPath (x:xs) = x </> concatPath xs

dropPrefix prefix path =
    let prefix' = splitOnPath prefix
        path'   = splitOnPath path
    in
        if (take (length prefix') path') == prefix' then
             Just $ concatPath $ drop (length prefix') path'
        else Nothing


filterJust :: Eq x => [Maybe x] -> [x]
filterJust = map (\(Just x) -> x) . filter (/= Nothing)
