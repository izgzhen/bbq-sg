module Config where
import BBQ.SG.Plugin
import BBQ.SG
import System.FilePath ((</>))
import qualified BBQ.SG.Plugin as P

rootURL     = "http://blog.zhenzhang.me"
-- rootURL = ".."

srcDir      = ".."


config = Config_ {
    _staticDir   = "../static",
    _srcDir      = srcDir,

    _postsSrc    = srcDir    </> "markdowns" </> "posts",
    _postsURL    = "posts",

    _pageSrc     = srcDir    </> "markdowns" </> "pages",
    _pageURL     = "pages",

    _wikiSrc     = srcDir    </> "markdowns" </> "wiki",
    _wikiURL     = "wiki",

    _imgSrc      = srcDir    </> "images",
    _imgURL      = "images",

    _jsSrc      = srcDir    </> "js",
    _jsURL      = "js",
    _jsAbsURL   = rootURL   </> "js",

    _cssSrc       = srcDir    </> "css",
    _cssURL       = "css",
    _cssAbsURL    = rootURL   </> "css",

    _tagsURL     = "tags",

    _modCache    = ".mod-cache",
    _blacklist   = "blacklist.txt",
    _analyticsId = "UA-64349949-1" 
}

myCopyRight = copyRight "Zhen Zhang" "2015"

goBackFromPost = P.a "Back to index page" "../index.html"

defaultAuthor  = "Zhen Zhang <izgzhen@gmail.com>"

