BBQ Static Generator
------

Status: Under refactoring

### Refactoring goals:

* Use `classy-prelude` instead standard prelude
* Use `Shake` build system as building tool, focus on intelligence, efficiency and parallelism
* Split between side effect and pure text transformation
* Use `Shakespeare` as default templating tool for posts etc.
* Try to use type-safe routes
* Re-export some String based libraries with Text
* Use `Vector` and `HashMap` more universally
* Make sure that the components code can't crush the builder (easily) as long as they passed the compilation

Since this refactoring focuses on better code, better safety and better performance, the old functionalities provided by BBQ-SG 0.4.X will stay.



### TODOs
* Try Wiki
* 


### Note
For the `FilePath`, URL etc., keep use the `String` Type. And try to move to the non-String type for content as long as possible

