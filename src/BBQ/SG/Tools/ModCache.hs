-- Modification Time Cache
module BBQ.SG.Tools.ModCache where
import System.FilePath
import qualified Data.Map as M
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import Data.Serialize
import Prelude hiding (readFile, writeFile)
import Data.ByteString.Lazy (readFile, writeFile)

type CacheMap = M.Map FilePath String

isNewEntry :: (FilePath, UTCTime) -> CacheMap -> Bool
isNewEntry (fp, tm) m = case M.lookup fp m of
                            Just str -> let tm' = read str :: UTCTime
                                        in tm' < tm
                            Nothing -> True

updateEntry :: (FilePath, UTCTime) -> CacheMap -> CacheMap
updateEntry (fp, tm) m = M.insert fp (show tm) m


loadCache :: FilePath -> IO (Maybe CacheMap)
loadCache path = do
    bs <- readFile path
    case decodeLazy bs of
        Right m -> return $ Just m
        Left errMsg -> do
            print $ "Error in loading cache: " ++ errMsg
            return Nothing

storeCache :: FilePath -> CacheMap -> IO ()
storeCache path cacheMap = writeFile path $ encodeLazy cacheMap

