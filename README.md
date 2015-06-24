BBQ-Static Generator
---

BBQ-SG is defined as teh static generator version of a full-fledged CMS. However, I am planning to write most code from scratch rather than inheriting code at the first statge. Maybe in the later period these two system can be combined together.

The key idea should be that, all information can be build on a single folder containing all markdown raw text and its related images, i.e., information should be self-contained rather than hashed into someother place like a DB.


## TODOs
* Re-attach meta info to post, esp. the title
* Meta info's email should be `Maybe`, and you should use `Either` in parsing
* Should write a markdown draft tool to make life easier
* Generalize the `index.html`, so user could provide index.page's content rather than `Main.hs`
* Maybe the `index.htm`'s generalization could be done with **generalization of "page" idea**
* Images needs a better sync scheme

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

## Page
`Page` should be written in Haskell DSL by user. A example for index could be like:

```haskell
import Plugin as P
index pages = do
  H.p $ "Welcome to my blog"
  P.urlList $ pages
```
  
