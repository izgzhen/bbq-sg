BBQ Static Generator
---

BBQ-SG is defined as a static generator of blog posts (not barbecue), like [mine hosted on GitHub Pages](http://blog.zhenzhang.me). It is organized as a library and user can write simple script to drive it.

The key idea behind this tool is that, the whole site can be rebuild from a single folder containing all markdown raw text and its related image. Information should be self-contained and always readable rather than hashed into some other place like a DB.

## Get started
I organized my own workflow like this:

Prepare two repos, `static` and `src`, though they might not be called like that, but you can `ln -s`.

In `src`, which has `markdowns` and `images` and some scripting `.hs` files which will call the library's interface with `config` attached. For example:

```haskell
import BBQ.SG
import Index (index)
import System.FilePath ((</>))

srcDir      = "."
staticDir   = "./static"
markdownDir = srcDir    </> "markdowns"
imgSrcDir   = srcDir    </> "images"
postsDir    = staticDir </> "posts"
imgStaDir   = staticDir </> "images"
analyticsId = "UA-64349949-1"

config = Config_ staticDir
                 markdownDir
                 imgSrcDir
                 postsDir
                 imgStaDir
                 analyticsId

main = runSG config index
```

The `Index.hs` is used to generate `index.html`, an example:

```haskell
module Index where
import BBQ.SG.Plugin as P

index posts = do
    P.p "Hello, welcome to my blog"
    P.urlList posts
```

But you can use the API in other way as well, enjoy blogging :)

## Tickets
* Maybe except for templates, all `H.XXX` should be provided by user?


## Features in planning
* Spell checking
* Content analyzing -- To generate tags, synopsis automatically
* Collect all URL/Images appeared in the posts
* Revision history by analyzing git commits
* Theme system


## Structure
You can think there are five abstract layers:

1. Driver
2. Interface
3. Components
4. Tools
5. Definitions

The users write `driver` code, and call the `interface`, which is composed of `components`, like `posts`, `tags`, `homepage` etc. But the realization of components needs a lot of tools, like `syncResource`, `parseMeta`, `getFileList` and `getMarkdowns`, which interacts with file system and processing texts, all nitty-gritty tools. The `Definitions` defines the configuration items and meta info of a post. Mostly declarations.





