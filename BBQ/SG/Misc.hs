module BBQ.SG.Misc where

import Control.Applicative((<$>))
import Control.Monad
import System.Directory
import System.FilePath (FilePath(..), (</>), takeExtensions, dropExtensions)
import System.Posix
import Data.List.Extra(splitOn)
import Data.List (group, sort)

unique :: (Eq a, Ord a) => [a] -> [a]
unique = map (\(x:_) -> x) . group . sort


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

getFileDict path = do
    contents <- (filter (\n -> n /= "." && n /= "..")) <$> getDirectoryContents path
    let contentsDict = zip (map (path </>) contents) contents
    dirs     <- filterM doesDirectoryExist $ map fst contentsDict
    files    <- filterM (doesFileExist . fst) contentsDict
    dict     <- concat <$> mapM getFileDict dirs
    return $ dict ++ files


type EitherS = Either String -- Left is the error info