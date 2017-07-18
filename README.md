Haki Cache
=====

Haki cache is a library that compiles your data into a static module
in Erlang for fast access. Haki was created because of a need for
storing _huge_ (megabytes of data) static state that is needed to be
shared between processes. 

This has obviously been done before, however most solutions do not
work well with megabytes of data. For reference have a look at the following:
 * [Mochiglobal](https://github.com/mochi/mochiweb/blob/master/src/mochiglobal.erl)
 * [Fling](https://github.com/basho-labs/fling) 
 * [Talk by Mark Allen about Fling](http://www.erlang-factory.com/static/upload/media/1459269312665211markallenwhenetsistooslow.pdf)
 * [Exploiting Constant Pool by Yoshihiro Tanaka](https://www.youtube.com/watch?v=hcGkT3Czd7U)
 
### Note
Some of the hacks used in this library are very risky and unstable. Please use 
at your own risk. I don't recommend using this library unless you have 
extensive tests around your runtime system and make sure it works
for your use case.

There is a reason it's called Hacky Cache after all.

### Usage
```erlang
haki:cache(some_key, "VALUE").
> ok

haki:get(some_key).
> "VALUE"
```
