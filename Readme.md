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


#### Sub note:
If things are working, we dont need to go about changing conventions. Though each new refactoring,
which is almost a daily activity, I try to fix these as I go.

### Postgres commands
If you need to restart.
``` sudo /etc/init.d/postgresql restart ```

### How to run the app on localhost
``` sudo route add -net 224.0.0.0 netmask 240.0.0.0 dev lo ``` 
``` sudo ifconfig lo multicast ``` 
This worked for me.  : ``` Linux pop-os 4.13.0-16-generic #19-Ubuntu SMP Wed Oct 11 18:35:14 UTC 2017 x86_64 x86_64 x86_64 GNU/Linux ```


