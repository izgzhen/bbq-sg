module Meta where
import Data.List.Extra (splitOn)
import Data.List(sort, group)
-- Haskell's Date lib is awkward to use

type Day   = Int
type Year  = Int
type Month = Int

data Date = Date_ {
    _day    :: Maybe Day,
    _month  :: Month,
    _year   :: Year
} deriving (Show, Eq)

data Email = Email String String deriving (Eq)

instance Show Email where
    show (Email name domain) = name ++ "@" ++ domain

data Contact = Contact_ {
    _name   :: String,
    _email  :: Email
} deriving (Show, Eq)

data Meta = Meta_ {
    _title  :: Maybe String,
    _date   :: Maybe Date,
    _author :: Maybe Contact,
    _tags   :: [String]
} deriving (Show, Eq)

--------------------------------------------------------

-- parseMeta :: String -> Maybe (Meta , String)
parseMeta str = do
    let lns = lines str
    -- First line is supposed to be title when second line is '---'
    case lns of
        title:"---":rest -> parseMetaItems (Meta_ (Just title) Nothing Nothing [], rest)
        rest             -> parseMetaItems (Meta_ Nothing      Nothing Nothing [], rest)

type Parser = (Meta, [String]) -> Maybe (Meta, String)

parseMetaItems :: Parser
parseMetaItems (m, lns) = do
    let f s = length s >= 1 && head s == '+'
    let items = takeWhile f lns
    let rest  = dropWhile f lns
    let items' = map (tail . words) items
    let parse  = (\x -> eat "Tags:" eatTags x >>= eat "Date:" eatDate >>= eat "Author:" eatAuthor )
    (m', _) <- parse (m, items')
    return (m', concat $ map (++ "\n") rest)

eat :: String -> (Meta -> [String] -> Maybe Meta) -> (Meta, [[String]]) -> Maybe (Meta, [[String]])
eat identifier eatF (m, lns) = do
    let (a, b) = span (\wds -> head wds /= identifier) lns
    case b of
        selected : notSelected -> do
            let info = tail selected
            m' <- eatF m info
            return (m', a ++ notSelected)
        _ -> return (m, a)

eatDate m@(Meta_ t _ a tg) wds =
    if length wds == 0 then Nothing
    else do
        d <- toDate $ head wds
        return $ Meta_ t (Just d) a tg

eatAuthor m@(Meta_ t d _ tg) nameAndEmail = do
    let (name, email) = (front nameAndEmail, last nameAndEmail)
    email' <- toEmail email
    let a = Contact_ (unwords name) email'
    return $ Meta_ t d (Just a) tg

eatTags  m@(Meta_ t d a _) xs = do
    let xs' = unique . map (clean ' ') . splitOn "," $ unwords xs
    return $ Meta_ t d a xs'

--------------------------------------------------------

toEmail :: String -> Maybe Email
toDate  :: String -> Maybe Date

toEmail str = case span (/= '@') str of
    (name, '@':domain) -> Just $ Email name' domain'
                           where name'   = clean '<' name
                                 domain' = clean '>' domain
    _ -> Nothing

toDate str = case splitOn "." str of
    (year:month:rest)     -> do
        year'  <- checkRange 0 10000 year
        month' <- checkRange 1 12    month
        case rest of
            []   -> return $ Date_ Nothing month' year'
            d:[] -> do
                day' <- checkRange 1 30 d
                return $ Date_ (Just day') month' year'
            _    -> Nothing
    _ -> Nothing

checkRange l h str = let i = read str :: Int
                     in if i >= l && i <= h then Just i
                         else Nothing

unique :: (Eq a, Ord a) => [a] -> [a]
unique = map (\(x:_) -> x) . group . sort


front = reverse . tail . reverse

clean x xs = let xs' = if head xs == x then tail xs else xs
             in if last xs' == x then front xs' else xs'
