module BBQ.SG.Meta where
import Data.List.Extra (splitOn)
import Data.List(sort, group)
import System.FilePath(FilePath)
import BBQ.SG.Misc
-- Haskell's Date lib is awkward to use

type Day   = Int
type Year  = Int
type Month = Int

data Date = Date_ {
    _day    :: Maybe Day,
    _month  :: Month,
    _year   :: Year
} deriving (Eq)

instance Show Date where
    show (Date_ d m y) = show y ++ "." ++ show m ++ showMaybe d

data Email = Email String String deriving (Eq)

instance Show Email where
    show (Email name domain) = "<" ++ name ++ " \\AT " ++ domain ++ ">"

data Contact = Contact_ {
    _name   :: String,
    _email  :: Email
} deriving (Eq)

instance Show Contact where
    show (Contact_ name email) = name ++ "  " ++ show email

data Meta = Meta_ {
    _title  :: Maybe String,
    _date   :: Maybe Date,
    _author :: Maybe Contact,
    _tags   :: [String],
    _path   :: FilePath
} deriving (Show, Eq)

