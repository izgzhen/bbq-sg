BBQ-Static Generator
---

BBQ-SG is defined as teh static generator version of a full-fledged CMS. However, I am planning to write most code from scratch rather than inheriting code at the first statge. Maybe in the later period these two system can be combined together.

The key idea should be that, all information can be build on a single folder containing all markdown raw text and its related images, i.e., information should be self-contained rather than hashed into some other place like a DB.

## Get started
I organized my workflow like this:

Prepare two repos, `static` and `src`, they might not be called that name, but you can `ln -s`.

In `src`, which has `markdowns` and `images` and some launch `.hs` files which will call the library's interface with `config` attached. For example:

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
* Meta info's email should be `Maybe`, and you should use `Either` in parsing
* Should write a markdown draft tool to make life easier
* Images needs a better sync scheme
* Use `metaDict` to generate some more indexing pages
* Try add some CSS & JS to your personal blog, so you can know if there is something to be done in managing more kinds of static resource

## Spec
1. Markdown post source format
2. Define the configuration in Haskell DSL
3. Layout in Hamlet
4. Use Haskell to maintain a meta-info db to support
	* Tagging
	* Archiving
4. Consider more amazing features
	* Spell Checking
	* Content analyzing -- To generate tags, synopsis automatically
	* Plugins
	* Link Hub -- Collect all URL appeared in the post
	* Review History by analyzing git commits
5. Combine with Dynamic BBQ
	* Real-time preview + Publish
	* Web-based editor
	* Theme system

