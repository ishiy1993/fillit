# fillit
[![Hackage](https://img.shields.io/hackage/v/fillit.svg)](http://hackage.haskell.org/package/fillit-0.1.0.0)
[![Travis](https://img.shields.io/travis/rust-lang/rust.svg)](https://travis-ci.org/ishiy1993/fillit)

Flexible String Substitution library

## Features

- Support two kinds of substitutions: required and optional.
- You can change the symbols for substitutions.

Note: This library does not support any sanitization.

## Usage
Example:

```
ghci> :set -XOverloadedStrings
ghci> import Data.Text.Fillit
ghci> import qualified Data.HashMap.Lazy as HM
ghci> let dic = HM.fromList [("name", "Tom"), ("age", "22")]
ghci> fill "$name$ (%age%)" dic
Right "Tom (22)"
ghci> fill "$name$ (%school%)" dic
Right "Tom (%school%)"
ghci> fill "$name$ ($school$)" dic
Left "There is no key in dict, such as school"
```

By default, the symbol `$` means a required substitution.
On the other hand, the symbol `%` means a optional substitution.
These symbols can be changed. See `fill'`.

## Development

```
$ git clone git@github.com:ishiy1993/fillit.git
$ cd fillit
$ stack build
$ stack test
```
