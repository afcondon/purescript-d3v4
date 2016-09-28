# purescript-generics

[![Latest release](http://img.shields.io/bower/v/purescript-generics.svg)](https://github.com/purescript/purescript-generics/releases)
[![Build Status](https://travis-ci.org/purescript/purescript-generics.svg?branch=master)](https://travis-ci.org/purescript/purescript-generics)
[![Dependency Status](https://www.versioneye.com/user/projects/5620cd8436d0ab00160009c5/badge.svg?style=flat)](https://www.versioneye.com/user/projects/5620cd8436d0ab00160009c5)

Generic programming.

## Usage

```
bower install purescript-generics
```

The methods in the `Generic` type class can be derived in versions >= 0.7.3 of the PureScript compiler with the following syntax:

``` purescript
derive instance genericMyType :: Generic MyType
```

There are some example usages of the library [in the tests](test/Main.purs).

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-generics).
