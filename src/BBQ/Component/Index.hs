module BBQ.Component.Index (
  indexCollector
) where

import BBQ.Import
import qualified Data.HashMap.Lazy as HM
import BBQ.Route
import BBQ.Task
import BBQ.Component.Post
import BBQ.Component.Wiki

indexCollector :: Collector
indexCollector = Collector {
    target   = "index.html",
    widgets  = ["post/widget.json", "wiki/widget.json"],
    resolver = indexResolver
}

indexResolver widgetMap =
    let mPostWidget = HM.lookup "post/widget.json" widgetMap
        mWikiWidget = HM.lookup "wiki/widget.json" widgetMap
    in do
        postWidget <- mPostWidget
        wikiWidget <- mWikiWidget
        wks <- wikis <$> decode wikiWidget
        posts <- allPosts <$> decode postWidget
        return "index"


