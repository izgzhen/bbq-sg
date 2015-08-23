{-# LANGUAGE DeriveGeneric #-}

-- Modification Time Cache
module BBQ.SG.Tools.ModCache where
import System.FilePath
import qualified Data.Map as M
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import Data.Serialize
import System.Directory
import Prelude hiding (readFile, writeFile, length)
import Data.ByteString.Char8 (ByteString, readFile, writeFile, length, pack, unpack)
import GHC.Generics (Generic)

newtype Time = Time { unTime :: ByteString } deriving (Eq, Show, Generic)

instance Serialize Time
instance Ord Time where
    compare (Time t1) (Time t2) = let t1' = read (unpack t1) :: UTCTime
                                      t2' = read (unpack t2) :: UTCTime
                                  in  compare t1' t2'


getModTime :: FilePath -> IO Time
getModTime p = do
    utcTime <- getModificationTime p
    return $ Time (pack $ show utcTime)

newtype CacheMap a = CacheMap {unCacheMap :: M.Map FilePath (Time, a)} deriving (Show, Eq)


isNewEntry :: Serialize a => FilePath -> Time -> CacheMap a -> Bool
isNewEntry fp tm (CacheMap m) = case M.lookup fp m of
                                    Just (tm', _) -> tm' < tm
                                    Nothing -> True

getEntryData :: Serialize a => FilePath -> CacheMap a -> Maybe a
getEntryData fp (CacheMap m) = do
    (rm, d) <- M.lookup fp m
    return d

updateEntryData :: Serialize a => FilePath -> a -> CacheMap a -> CacheMap a
updateEntryData fp a (CacheMap m) = case M.lookup fp m of
    Just (tm, d) -> updateEntry fp tm a (CacheMap m)
    Nothing      -> error "Can't update a null entry"


updateEntry :: Serialize a => FilePath -> Time -> a -> CacheMap a -> CacheMap a
updateEntry fp tm d (CacheMap m) = CacheMap $ M.insert fp (tm, d) m


loadCache :: Serialize a => FilePath -> IO (Maybe (CacheMap a))
loadCache path = do
    bs <- readFile path
    if length bs == 0 then
        return $ Just (CacheMap M.empty)
        else case decode bs of
            Right m -> return $ Just (CacheMap m)
            Left errMsg -> do
                putStrLn $ "Error in loading cache: " ++ errMsg
                return Nothing

storeCache :: Serialize a => FilePath -> CacheMap a -> IO ()
storeCache path (CacheMap m) = writeFile path $ encode m


