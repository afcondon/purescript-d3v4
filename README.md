# From the ground up re-implementation of purescript-d3

Key points:

* same DSL approach as @pelotom's original, but...
* ...no typeclasses (so far)
* latest version of D3, ie v4
* ADTs for the polymorphic parameter types with resulting explicit representation in the DSL
* purescript-eff-functions to wrap all the underlying calls
