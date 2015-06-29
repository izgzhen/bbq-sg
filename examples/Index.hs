module Index where
import BBQ.SG.Plugin as P

indexLayout metas = do
    P.p "Hello, welcome to my blog"
    P.a "Checkout tags" "tags.html"
    let posts = map (\meta -> (showMaybeStr $ _title meta, _path meta)) metas
    P.urlList posts

