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

You can also group a set of key/values into a single module by using the bucketing feature.
You have to pass an atom for the bucket name and a map of `atom() => any()` for caching:
```erlang
Erlang/OTP 20 [erts-9.0] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V9.0  (abort with ^G)
1> haki:cache_bucket(people, #{
1>     mike => #{age => 10, height => 180, weight => 65},
1>     christina => #{age => 10, height => 160, weight => 45}
1> }).
ok

2> haki:get(people, mike).
#{age => 10,height => 180,weight => 65}

3> haki:get(people, christina).
#{age => 10,height => 160,weight => 45}

4> haki:get(people, no_name).
bad_key

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
1> haki:cache(some_haki_key, "VALUE", #{compiler => haki_beam_compiler}).
ok

2> haki:get(some_haki_key).
"VALUE"
```

### Benchmarks
Lots of small/medium sized keys:
```erlang
file size: 1781 MB
num keys: 4554
avg. key length: 340

lists:foreach(
  fun(K) ->
     haki:cache(K, maps:get(K, Map), #{compiler => haki_beam_compiler})
  end, maps:keys(Map)).
  
Caching of 4554 keys took 1 minute and 55 seconds.
```
Few large keys:
```erlang
file size: 177.319806098938Mb
num keys: 3
avg. key length: 27000

haki_asm_compiler: 49,646ms
haki_beam_compiler: 1,389ms
```

### Troubleshooting
If you are loading a massive amount of data, you may run into this error:
```
literal_alloc: Cannot allocate ____ bytes of memory (of type "literal").
```

This is because Erlang VM by default only allows for 1GB of literal data loaded in the VM (http://erlang.org/doc/man/erts_alloc.html). 
To go around this, you can add `+MIscs <size in MB>` to your VM configs at runtime and everything should work (keep in mind this is only available on 64-bit machines). 

I suggest you using 2x of the amount of data you are expecting to load into the VM, this way your VM will not crash if you are reloading or rebuilding the cache.
