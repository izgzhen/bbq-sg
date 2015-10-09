module BBQ.Import (
  module BBQ.Config
, module Text.Hamlet
, module Data.HashMap.Lazy
, module ClassyPrelude
, getGitDate
, error_
, readFile'
, writeFile'
, eitherToMaybe
, renderHtml
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
import qualified Development.Shake.FilePath as SFP
import qualified Data.Text.Lazy as TL

getGitDate p = do
    let gitCmd = "git log -1 --format=%ci --" :: String
    Stdout gitDate <- cmd gitCmd [unpack p]
    return (pack gitDate)

error_ :: Text -> Action ()
error_ t = error $ unpack t

eitherToMaybe e = case e of
    Left _  -> Nothing
    Right x -> Just x


readFile'  fp   = do
    putNormal $ "[DEBUG] reading " ++ fp
    pack <$> S.readFile' fp

writeFile' fp t = S.writeFile' fp (unpack t)


renderHtml = TL.toStrict . BLZ.renderHtml