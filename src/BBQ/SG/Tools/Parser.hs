module BBQ.SG.Tools.Parser (parseMeta) where
import BBQ.SG.Meta
import Data.List.Split (splitOn)
import BBQ.SG.Misc

type Parser = (Meta, [String]) -> EitherS (Meta, String)

parseMeta str = do
    let lns = lines str
    -- First line is supposed to be title when second line is '---'
    case lns of
        title:"---":rest -> parseMetaItems (Meta_ (Just title) Nothing Nothing [] "", rest)
        rest             -> parseMetaItems (Meta_ Nothing      Nothing Nothing [] "", rest)



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



