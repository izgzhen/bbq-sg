module BBQ.SG.Meta where
import Data.List.Extra (splitOn)
import Data.List(sort, group)
import System.FilePath(FilePath)
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

--------------------------------------------------------

-- parseMeta :: String -> Maybe (Meta , String)
parseMeta str = do
    let lns = lines str
    -- First line is supposed to be title when second line is '---'
    case lns of
        title:"---":rest -> parseMetaItems (Meta_ (Just title) Nothing Nothing [] "", rest)
        rest             -> parseMetaItems (Meta_ Nothing      Nothing Nothing [] "", rest)

type EitherS = Either String -- Left is the error info

type Parser = (Meta, [String]) -> EitherS (Meta, String)

parseMetaItems :: Parser
parseMetaItems (m, lns) = do
    let f s = length s >= 1 && head s == '+'
    let items = takeWhile f lns
    let rest  = dropWhile f lns
    let items' = map (tail . words) items
    let parse  = (\x -> eat "Tags:" eatTags x >>= eat "Date:" eatDate >>= eat "Author:" eatAuthor )
    (m', _) <- parse (m, items')
    return (m', concat $ map (++ "\n") rest)

eat :: String -> (Meta -> [String] -> EitherS Meta) -> (Meta, [[String]]) -> EitherS (Meta, [[String]])
eat identifier eatF (m, lns) = do
    let (a, b) = span (\wds -> head wds /= identifier) lns
    case b of
        selected : notSelected -> do
            let info = tail selected
            m' <- eatF m info
            return (m', a ++ notSelected)
        _ -> return (m, a)

eatDate m@(Meta_ t _ a tg p) wds =
    if length wds == 0 then return m
    else do
        d <- toDate $ head wds
        return $ Meta_ t (Just d) a tg p

eatAuthor m@(Meta_ t d _ tg p) nameAndEmail = do
    let (name, email) = (front nameAndEmail, last nameAndEmail)
    email' <- toEmail email
    let a = Contact_ (unwords name) email'
    return $ Meta_ t d (Just a) tg p

eatTags  m@(Meta_ t d a _ p) xs = do
    let xs' = unique . map (clean ' ') . splitOn "," $ unwords xs
    return $ Meta_ t d a xs' p

--------------------------------------------------------

toEmail :: String -> EitherS Email
toDate  :: String -> EitherS Date

toEmail str = case span (/= '@') str of
    (name, '@':domain) -> return $ Email name' domain'
                           where name'   = clean '<' name
                                 domain' = clean '>' domain
    _ -> fail $ "email \"" ++ str ++ "\" is not legal"

toDate str = case splitOn "." str of
    (year:month:rest)     -> do
        year'  <- checkRange 0 10000 year
        month' <- checkRange 1 12    month
        case rest of
            []   -> return $ Date_ Nothing month' year'
            d:[] -> do
                day' <- checkRange 1 30 d
                return $ Date_ (Just day') month' year'
            _    -> fail $ "date \"" ++ str ++ "\" is not legal"
    _ -> fail $ "date \"" ++ str ++ "\" is not legal"

checkRange l h str = let i = read str :: Int
                     in if i >= l && i <= h then return i
                         else fail $ str ++ " is not in range (" ++ show l ++ ", " ++ show h ++ ")"

unique :: (Eq a, Ord a) => [a] -> [a]
unique = map (\(x:_) -> x) . group . sort


front = reverse . tail . reverse

clean x xs = let xs' = if head xs == x then tail xs else xs
             in if last xs' == x then front xs' else xs'

showMaybe Nothing   = ""
showMaybe (Just a) = show a


showMaybeStr Nothing  = ""
showMaybeStr (Just s) = s