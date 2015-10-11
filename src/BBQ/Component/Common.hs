module BBQ.Component.Common (
  markDownExtract
, parseCreatedDate
, parseModifiedDate
, pandocMetaToTitle
) where

-- Some commonly used utilities
import BBQ.Import
import Data.Time.ISO8601
import Text.Pandoc

markDownExtract :: forall c c1 t.
                   (IsString [Element c1], IsSequence t, MonoFoldable c,
                    MonoFoldable c1, Element c ~ Char, Element t ~ Char,
                    Element c1 ~ Char) =>
                   c
                   -> c1 -> FilePath -> Maybe (FilePath, t, UTCTime, UTCTime, Pandoc)

markDownExtract text gitDate path = do
    pandoc@(Pandoc meta _) <- eitherToMaybe $ readMarkdown def (unpack text)
    let title = pandocMetaToTitle meta
    let name  = takeFileName $ dropExtension path
    cDate <- parseCreatedDate name
    mDate <- parseModifiedDate gitDate cDate
    return (name, title, cDate, mDate, pandoc)

parseCreatedDate :: [Char] -> Maybe UTCTime
parseCreatedDate x = parseISO8601 . (++"T00:00:00Z") -- Append time
                                  . intercalate "-" . take 3 $ splitOn "-" x
                                 -- Filename starts with 'year-month-day'


parseModifiedDate :: forall c.
                           (IsString [Element c], MonoFoldable c, Element c ~ Char) =>
                           c -> UTCTime -> Maybe UTCTime
parseModifiedDate gitDate cDate =
    case lines (unpack gitDate) of
       []    -> return cDate -- If not checked in yet
       (x:_) -> parseISO8601
                . (\[d,t,z] -> d ++ "T" ++ t ++ z)
                $ words x
             -- Convert to proper ISO8601 date

pandocMetaToTitle :: forall c.
                   (IsSequence c, Element c ~ Char) =>
                   Meta -> c
pandocMetaToTitle meta = pack $ writeAsciiDoc def (Pandoc nullMeta [Plain (docTitle meta)])


