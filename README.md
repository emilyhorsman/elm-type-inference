# elm-type-inference

[![Build Status](https://travis-ci.org/emilyhorsman/elm-type-inference.svg?branch=master)](https://travis-ci.org/emilyhorsman/elm-type-inference)

## Updating `src/Elm`

This directory includes Haskell modules of type declaration strings from `elm/core`.

```
$ cd vendor
$ git clone https://github.com/elm/core.git
$ cd ..
$ python3 FilterTypeAnnotations.py vendor/core/src/Basics.elm > src/Elm/Basics.hs
```
