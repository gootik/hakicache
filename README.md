Haki Cache [![Build Status](https://travis-ci.org/gootik/hakicache.svg?branch=master)](https://travis-ci.org/gootik/hakicache)
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
Erlang/OTP 20 [erts-9.0] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V9.0  (abort with ^G)
1> haki:cache(some_key, "VALUE").
ok

2> haki:get(some_key).
"VALUE"
```

### Super experimental features
The `haki_beam` compiler skips all the steps that is required
to go from a `.S` file to a binary representation of the module.
The specifics of the BEAM file format is available here: https://happi.github.io/theBeamBook/#CH-beam_modules.
In OTP20 we can piggy back the `beam_asm` module and directly get a 
binary BEAM format. This is how `haki_beam_compiler` works and it is only
available in OTP20: 
```erlang
Erlang/OTP 20 [erts-9.0] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V9.0  (abort with ^G)
1> haki:cache(some_haki_key, "VALUE", haki_beam_compiler).
ok

2> haki:get(some_haki_key).
"VALUE"
```

### Benchmark
```erlang
file size: 1781 MB
num keys: 4554
avg. key length: 340

lists:foreach(
  fun(K) ->
     haki:cache(K, maps:get(K, Map), haki_beam_compiler)
  end, maps:keys(Map)).
  
Caching of 4554 keys took 2 minutes.
```
