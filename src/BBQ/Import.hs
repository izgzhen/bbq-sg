module BBQ.Import (
  module BBQ.Config
, module Text.Hamlet
, module Data.HashMap.Lazy
, module ClassyPrelude
, error_
, readFile'
, writeFile'
, eitherToMaybe
, renderHtml
, newCache'
, Widget
, Widgets
, module Data.List.Split
, module Text.Pandoc
, module Development.Shake
, module Development.Shake.FilePath
, module Control.Monad.Except
, module BBQ.Route
) where

import BBQ.Route
import BBQ.Config
import Text.Hamlet
import Data.HashMap.Lazy (HashMap)
import ClassyPrelude
import Control.Monad.Except (ExceptT, runExceptT, throwError, Except)
import qualified Text.Blaze.Html.Renderer.Text as BLZ
import Development.Shake hiding (readFile', writeFile', Env, (*>))
import Development.Shake.FilePath
import qualified Development.Shake as S
import Development.Shake.FilePath
import qualified Data.Text.Lazy as TL
import Text.Pandoc (Pandoc)
import Data.List.Split (splitOn)

error_ :: Text -> Action ()
error_ t = error $ unpack t

eitherToMaybe :: forall t a. Either t a -> Maybe a
eitherToMaybe e = case e of
    Left _  -> Nothing
    Right x -> Just x

readFile' :: forall b.
                   (IsSequence b, Element b ~ Char) =>
                   String -> Action b
readFile'  fp   = do
    putNormal $ "[DEBUG] reading " ++ fp
    pack <$> S.readFile' fp

writeFile' :: forall c.
            (MonoFoldable c, Element c ~ Char) =>
            FilePath -> c -> Action ()
writeFile' fp t = S.writeFile' fp (unpack t)

renderHtml :: Html -> Text
renderHtml = TL.toStrict . BLZ.renderHtml

type Widget  = HtmlUrl WebPath
type Widgets = HashMap Text Widget


newCache' :: forall v. Action v -> Rules (Action v)
newCache' action = do
    cache <- newCache $ \() -> action
    return (cache ())

