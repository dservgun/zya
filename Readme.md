# Synopsis

A realtime webserver allowing users to store and forward custom messages.

## [Protocol](Protocol.md)

## [![Build Status](https://travis-ci.org/dservgun/zya.svg?branch=master)](https://travis-ci.org/dservgun/zya)


## Helpful commands

``` cabal sandbox init ```
``` cabal install --only-dependencies --enable-tests ```

### Variable naming conventions:
Trying to follow haskell conventions. Though, if a variable is needed in the method signature, I am
using smalltalkish convention: for example

f :: T -> T
f aT = aT

if the above function is written as below:

f = \aL -> aL

in the latter case the variable is curried, and aL(ocal). Though, we should probably use a sequence, which
would then look as below

f =\a1 -> a1

I dont like 0 based arrays. Actually, I dont like this anyway, doesnt read well.

#### Sub note:
If things are working, we dont need to go about changing conventions. Though each new refactoring,
which is almost a daily activity, I try to fix these as I go.

### Postgres commands
If you need to restart.
``` sudo /etc/init.d/postgresql restart ```

