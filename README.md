# purescript-d3v4
## From the ground up re-implementation of purescript-d3

### Key points:

* same DSL approach as @pelotom's original, but...
* ...fewer typeclasses (so far)
* latest version of D3, ie v4
* ADTs for the polymorphic parameter types with resulting explicit representation in the DSL
* purescript-eff-functions to wrap all the underlying calls (instead of easy-ffi)

### Further thoughts

D3 encodes a _wealth_ of domain specific information about data visualization that is the fruit of both creator Mike Bostock's previous visualization libraries and, also, the extraordinarily rich eco-system of users that it has accumulated.

It presents an interface and documentation of such depth that even JavaScript neophytes or, in some cases, non-programmers can take it and produce the graphics and even animations or interactive visualizations that they require.

By being very largely decoupled from the actual presentation technology of HTML DOM / SVG / Canvas etc it both enables users to very often defer learning those technologies and enables people with expertise in those technologies to work with visualizations without learning very much if anything about D3.

In essence, D3 presents a Domain Specific Language (DSL) embedded in JavaScript for the creation of visualizations. Functional programming languages such as Haskell and Purescript, are ideally suited to the creation of DSLs and [purescript-d3](https://github.com/pelotom/purescript-d3) by [Tom Crockett](https://github.com/pelotom) demonstrated how with a couple of operator aliases and wrappers for D3 functions one could very nearly replicate the form of the JavaScript "DSL" that is D3 in Purescript.

### Objectives

This repo is attempting to find a balance between two objectives that are certainly in tension with one another:

1. define a DSL in Purescript that is so close to the JavaScript D3 style that one can read and perhaps write it without resorting to reference material
2. leverage the undoubted advantages of strong static typing and immutability to make it easier to program by making invalid programs harder to express

A third objective is to ensure that this repo sits well within the Purescript library ecosystem, eschewing redundant definitions of, for example DOM, Events, Dates etc and sitting nicely within web apps built using Pux, Thermite, Halogen etc.

A fourth objective is to make explicit much of the D3 APIs (whose semantics is so much part defined only by, admittedly very good, documentation) in such a way that they _may_ suggest reformulations into a more powerful / expressive functional programming style *without losing* the domain specific knowledge that D3 represents.
