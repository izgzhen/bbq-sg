module BBQ.FTree (
  mkFileTree
, PathTree
, filterFiles
, filterDirs
, filterFTree
, FTree(..)
) where

import System.Directory
import System.FilePath
import Control.Monad
import Data.Maybe
import Prelude

data FTree x = Dir x [FTree x]
             | Regular x
             deriving (Show)

filterFTree :: forall x. (x -> Bool) -> FTree x -> FTree x
filterFTree f x = case x of
        Dir n ts  -> Dir n $ catMaybes $ map f' ts
        other     -> other
    where
        f' (Regular n) = if f n then Just (Regular n) else Nothing
        f' dir         = Just $ filterFTree f dir

filterFiles :: [FTree x] -> [x]
filterFiles = catMaybes . map (\d -> case d of
                                Regular x -> Just x
                                _         -> Nothing)

filterDirs :: [FTree x] -> [FTree x]
filterDirs  = filter (\d -> case d of
                        Dir _ _ -> True
                        _       -> False)

type PathTree = FTree FilePath


mkFileTree :: FilePath -> IO PathTree
mkFileTree root = do
    contents <- filter (\d -> d /= ".." && d /= ".") <$> getDirectoryContents root
    subDirs  <- filterM doesDirectoryExist (map (root </>) contents)
    subFiles <- filterM doesFileExist (map (root </>) contents)
    subTrees <- mapM mkFileTree subDirs
    return $ Dir root (subTrees ++ map Regular subFiles)

