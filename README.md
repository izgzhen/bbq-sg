BBQ Static Generator
---

BBQ-SG is defined as a static generator of blog posts (not barbecue), like [mine hosted on GitHub Pages](http://blog.zhenzhang.me). It is organized as a library and user can write simple scripts to drive it.

The key idea behind this tool is that, **the whole site can be rebuild from a single folder containing all markdown raw text and its related resource like js and css**. In my design, raw text should be as *self-contained* as possible and always *human-readable* rather than hashed into some other place like a DB.

Except for that, it also emphasizes the extensibility and openness. The project is always going towards neat separation of core logic, layout and user content.

When I completed the Cache system and Theme system and have a more usable version, I might write a post about this generator.

## Getting started
I organized my own workflow like this:

I used GitHub Pages service so I have a repo for all static stuff, like this for example:

    .
    ├── CNAME
    ├── css
    │   └── ...
    ├── images
    │   ├── ...
    ├── index.html
    ├── js
    │   └── ...
    ├── posts
    │   ├── ...
    └── tags.html

And I have a source folder including all launching scripts, layout script, markdowns, js, css and images:

    .
    ├── Boot.hs
    ├── Config.hs
    ├── Index.hs
    ├── Main.hs
    ├── Posts.hs
    ├── Tags.hs
    ├── blacklist.txt
    ├── css
    │   └── ...
    ├── images
    │   ├── ...
    ├── js
    │   └── ...
    ├── markdowns
    │   ├── ...
    └── static -> /path/to/my/static/repo

The `blacklist.txt` can contain the common words you might want to pre-filter out when using the keyword-auto-generator, although the generator will do some filtering itself as well.

The `Boot.hs` is a separated written script to pre-generate a markdown file containing the required format for meta-info to be parsed. It is pretty useful since you don't have to fill in the date etc. by yourself.

The `Posts.hs`, `Index.hs` and `Tags.hs` etc. are all layout scripts. For examples you can refer to the `examples` in this repo. And `Main.hs` drive the generation of static blog, with `Config.hs` in which the paths, google analytics id etc. filled in.

Although this is the workflow I designed the `BBQ-SG` with in mind, but you are free to choose another style. Happy blogging :)

## Tickets
* Theme system
* Cache system
* The size of picture should be limited ...
* The js/css usage should be more flexible

## FUTURE
+ Revision history by analyzing git commits

## Structure
You can think there are five abstract layers:

1. Driver
2. Interface
3. Components
4. Tools
5. Definitions

The users write `driver` code, and call the `interface`, which is composed of `components`, like `posts`, `tags`, `homepage` etc. But the realization of components needs a lot of tools, like `syncResource`, `parseMeta`, `getFileList` and `getMarkdowns`, which interacts with file system and processing texts, all nitty-gritty tools. The `Definitions` defines the configuration items and meta info of a post. Mostly declarations.


## Cache system
Use the dependency pair to facilitate the cache write system.

We can expose a handler return readers `[Reader]` and a writer `Writer`. When the Monad is evaluated, the IO will first get cache table and compare with readers, if all readers are not updated, then the writer will be discarded; or it will do the actual writing. Remember, due to laziness, when the writer is discarded, the read will not happen as well.

## Theme system




