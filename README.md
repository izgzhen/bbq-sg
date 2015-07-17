BBQ Static Generator
---

BBQ-SG is defined as a static generator of blog posts (not barbecue), like [mine hosted on GitHub Pages](http://blog.zhenzhang.me). It is organized as a library and user can write simple script to drive it.

The key idea behind this tool is that, the whole site can be rebuild from a single folder containing all markdown raw text and its related image. Information should be self-contained and always readable rather than hashed into some other place like a DB.

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

The `Boot.hs` is a seperated written script to pre-generate a markdown file containing the required format for meta-info to be parsed. It is pretty useful since you don't have to fill in the date etc. by yourself.

The `Posts.hs`, `Index.hs` and `Tags.hs` are all layout scripts. For examples you can refer to the `examples` in this repo. And `Main.hs` drive the generation of static blog, with `Config.hs` in which the paths, google analytics id etc. filled in.

Although this is the workflow I designed the `BBQ-SG` with in mind, but you are free to choose another style. Happy blogging :)

## Tickets

* Auto Hash-tagging
* Generate split tag pages and do reverse linking (Need to change the single phase to multi-phase design, more flexible coding style)
* How to organize the `src` dir, `sta` dir as well as the path in HTML? They have some relationships, but a little difficult to get cleaner while don't cause too much trouble to user.
* Better layout framework: Give user enough choices while not complicating the APIs (Should be intuitive)

## Features in planning
* Collect all URL appeared in the posts
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

