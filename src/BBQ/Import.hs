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
, module Development.Shake
, module Development.Shake.FilePath
, module Control.Monad.Except
) where


import BBQ.Config
import Text.Hamlet
import Data.HashMap.Lazy (HashMap)
import ClassyPrelude
import Control.Monad.Except (ExceptT, runExceptT, throwError, Except)

import Development.Shake hiding (readFile', writeFile', Env, (*>))
import Development.Shake.FilePath
import qualified Development.Shake as S
import qualified Development.Shake.FilePath as SFP

getGitDate p = do
    let gitCmd = "git log -1 --format=%ci --" :: String
    Stdout gitDate <- cmd gitCmd [unpack p]
    return (pack gitDate)

error_ :: Text -> Action ()
error_ t = error $ unpack t

eitherToMaybe e = case e of
    Left _  -> Nothing
    Right x -> Just x


readFile'  fp   = pack <$> S.readFile' fp
writeFile' fp t = S.writeFile' (unpack t) fp