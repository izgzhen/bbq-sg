module BBQ.Component.Index (
  indexCollector
) where

-- import BBQ.Import
import BBQ.Task

indexCollector :: Collector
indexCollector = Collector {
    target   = "index.html",
    widgets  = ["post/widget.json", "wiki/widget.json"]
}



