module BBQ.Component.Post (
  postTask
) where

import BBQ.Import

import Data.Time (UTCTime)
import Language.Haskell.TH
import BBQ.Task
import qualified Data.HashMap.Lazy as HM
import BBQ.Component.Common

data PostMeta = PostMeta {
  pid     :: PostId
, title   :: Text
, date    :: UTCTime
, body    :: Pandoc
, tags    :: [Text]
}

data PostExtra = PostExtra {
  related :: [PostId]
}

data PostSummary = PostSummary {
  categories :: HashMap Text [PostId] -- Tag -> Posts
}

