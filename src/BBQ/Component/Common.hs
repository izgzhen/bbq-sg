module BBQ.Component.Common (
  markDownExtract
, parseModifiedDate
, pandocMetaToTitle
) where

import BBQ.Import
import Data.Time.ISO8601
import Text.Pandoc

markDownExtract :: Text -> Text -> FilePath -> Maybe (FilePath, Text, Maybe UTCTime, Pandoc)

markDownExtract text gitDate path = do
    pandoc@(Pandoc meta _) <- eitherToMaybe $ readMarkdown def (unpack text)
    let title = pandocMetaToTitle meta
    let name  = takeFileName $ dropExtension path
    let mDate = parseModifiedDate gitDate
    return (name, title, mDate, pandoc)

parseModifiedDate :: Text -> Maybe UTCTime
parseModifiedDate gitDate =
    case lines (unpack gitDate) of
       []    -> Nothing
       (x:_) -> parseISO8601 . (\[d,t,z] -> d ++ "T" ++ t ++ z) $ words x

pandocMetaToTitle :: Meta -> Text
pandocMetaToTitle meta = pack $ writeAsciiDoc def (Pandoc nullMeta [Plain (docTitle meta)])


