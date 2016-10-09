# purescript-d3v4
## From the ground up re-implementation of purescript-d3

### Key points:

* same DSL approach as @pelotom's original, but...
* ...fewer typeclasses (so far)
* latest version of D3, ie v4
* ADTs for the polymorphic parameter types with resulting explicit representation in the DSL
* purescript-eff-functions to wrap all the underlying calls (instead of easy-ffi)

### Installing and running

* Clone repo
* npm install
* bower install
* npm run build
* npm run
* npm run example-five-circles
* npm run example-gupIII
* npm run example-barchart
* npm run example-forcelayout
* npm run example-default

If you serve http from the examples directory you should be able to see all five of these simple demos run now.

### Some resources about visualization generally...

All of Edward Tufte's books are essential but [this](https://www.amazon.com/Envisioning-Information-Edward-R-Tufte/dp/0961392118) is maybe a good one to start with  in this context.

Very nice [book](https://www.amazon.com/Book-Trees-Visualizing-Branches-Knowledge/dp/1616892188) by Manuel Lima about the ubiquitous tree diagram, all the way back to Ramon Llull in 13thC. [Visual Complexity](https://www.amazon.com/Visual-Complexity-Mapping-Patterns-Information/dp/1616892196) by the same author is also great and perhaps even more relevant to some of the kinds of uses i anticipate for purescript-d3v4

Comprehensive overview / [introduction](https://www.amazon.com/Visualization-Analysis-Design-Peters-Series/dp/1466508914) to data visualization by the great Tamara Munzner.

![Envisioning Information cover image][1]
![The Book of Trees][2]
![Visual Complexity][3]
![Visualization Analysis][4]
[1]: ./EI.jpg
[2]: ./BoT.jpg
[3]: ./VC.jpg
[4]: ./VAaD.jpg

### ...and D3 specifically

[General Update Pattern:](http://bl.ocks.org/mbostock/3808234)

[How Selections Work (as it was in d3v3)](https://bost.ocks.org/mike/selection/)

[Irene Ros D3v4 -What’s New?](https://iros.github.io/d3-v4-whats-new/)

[Simpson's Paradox](http://vudlab.com/simpsons/) illustrated and explained using a variety of D3 ideas to very good effect.

$ubscription video course (no affiliation) on D3 with excellent free newsletter: [DashingD3](https://www.dashingd3js.com)

### Further thoughts

D3 encodes a _wealth_ of domain specific information about data visualization that is the fruit of both creator Mike Bostock's previous visualization libraries and, also, the extraordinarily rich eco-system of users that it has accumulated.

It presents an interface and documentation of such depth that even JavaScript neophytes or, in some cases, non-programmers can take it and produce the graphics and even animations or interactive visualizations that they require.

By being very largely decoupled from the actual presentation technology of HTML DOM / SVG / Canvas etc it both enables users to very often defer learning those technologies and enables people with expertise in those technologies to work with visualizations without learning very much if anything about D3.

In essence, D3 presents a Domain Specific Language (DSL) embedded in JavaScript for the creation of visualizations. Functional programming languages such as Haskell and Purescript, are ideally suited to the creation of DSLs and [purescript-d3](https://github.com/pelotom/purescript-d3) by [Tom Crockett](https://github.com/pelotom) demonstrated how with a couple of operator aliases and wrappers for D3 functions one could very nearly replicate the form of the JavaScript "DSL" that is D3 in Purescript.

### Objectives

This repo is attempting to find a balance between the following objectives (*NB* the first two are certainly in tension with one another to some extent):

1. define a DSL in Purescript that is so close to the JavaScript D3 style that one can read and perhaps write it without resorting to reference material
2. leverage the undoubted advantages of strong static typing and immutability to make it easier to program by making invalid programs harder to express and error values such as undefined and null explicit
3. a third objective is to ensure that this repo sits well within the Purescript library ecosystem, eschewing redundant definitions of, for example DOM, Events, Dates etc and sitting nicely within web apps built using Pux, Thermite, Halogen etc.˛
4. a fourth objective is to make explicit much of the D3 APIs (much of whose semantics is defined only by, admittedly very good, documentation) in such a way that they _may_ suggest reformulations into a more powerful / expressive functional programming style *without losing* the domain specific knowledge that D3 represents.

This last perhaps needs some elaborating. There's an open question in my mind as to how far it is useful to represent the underlying D3 API in a strict language. Many, perhaps a majority, of D3's API endpoints take the form of polymorphic functions which can be called with varying numbers and types of parameters. This is key to reducing the visual noise of the "DSL" and a big part of JavaScript's appeal to the programmer. It's also a big, big source of complexity in debugging and understanding of others' code or your own code after elapsed time.

In this current draft, i've added some visual noise by making the parameters explicit using ADTs while retaining the simplicity of the function name. An alternative, seen in `purescript-d3`, is to provide variations on the function name (`attr`, `attr'`, `attr''` etc). Another alternative would be to explicitly model the optionality of the parameters with `Maybe`s and `Either`s and `Nullable`s. Another alternative might be to do something clever with lenses.

#### Ex 1. Making parameter types explicit with ADTs (`purescript-d3v4`)

  ``` haskell
  circles <- g ... selectAll "circle"
      .. dataBind (Data circleData)
    .. enter .. append "circle"
      .. attr "cx" (AttrFn (\d i nodes el -> pure d.x))
      .. attr "cy" (AttrFn (\d i nodes el -> pure d.y))
      .. attr "r"  (SetAttr 20.0)
      .. style "stroke" (Value "black")
      .. style "fill"   (Value "red")
   ```


#### Ex 2. Providing variations on function names (`purescript-d3`)

  ``` haskell
  rootSelect ".chart"
    .. selectAll "div"
      .. bindData array
    .. enter .. append "div"
      .. style' "width" (\d -> show (x d) ++ "px")
      .. text' show
   ```


#### Ex 3. Explicitly modelling optionality of params (not implemented)


#### Ex 4. (something clever with lenses)


### _TODO - write more on the pros and cons of these four approaches._

Beyond these four stylistic choices there are, i intuit, possibilities for perceiving and exploiting deeper abstractions and perhaps formulating a more powerful DSL entirely, perhaps one that is sufficiently powerful to rival D3's own.
