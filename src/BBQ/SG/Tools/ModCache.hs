-- Modification Time Cache
module BBQ.SG.Tools.ModCache where
import System.FilePath
import qualified Data.Map as M
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import Data.Serialize
import Prelude hiding (readFile, writeFile, length)
import Data.ByteString (readFile, writeFile, length)

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
    if length bs == 0 then
        return $ Just M.empty
        else case decode bs of
            Right m -> return $ Just m
            Left errMsg -> do
                putStrLn $ "Error in loading cache: " ++ errMsg
                return Nothing

storeCache :: FilePath -> CacheMap -> IO ()
storeCache path cacheMap = writeFile path $ encode cacheMap


