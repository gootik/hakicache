

# Module haki #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

A static cache storage for Erlang.

<a name="description"></a>

## Description ##

Currently consists of two compilers:
ASM - Used for large lists as value. Read the module docs
for more info.

Syntax - Used for all other cases.

BEAM - This is experimental and only works for OTP20+
<a name="types"></a>

## Data Types ##




### <a name="type-cache_bucket_name">cache_bucket_name()</a> ###


<pre><code>
cache_bucket_name() = atom()
</code></pre>




### <a name="type-cache_bucket_value">cache_bucket_value()</a> ###


<pre><code>
cache_bucket_value() = #{<a href="#type-cache_key">cache_key()</a> =&gt; <a href="#type-cache_value">cache_value()</a>}
</code></pre>




### <a name="type-cache_key">cache_key()</a> ###


<pre><code>
cache_key() = atom()
</code></pre>




### <a name="type-cache_module_name">cache_module_name()</a> ###


<pre><code>
cache_module_name() = atom()
</code></pre>




### <a name="type-cache_options">cache_options()</a> ###


<pre><code>
cache_options() = #{compiler =&gt; <a href="#type-compiler">compiler()</a> | haki_default_compiler, save_snapshot =&gt; boolean()}
</code></pre>




### <a name="type-cache_value">cache_value()</a> ###


<pre><code>
cache_value() = any()
</code></pre>




### <a name="type-compile_ret">compile_ret()</a> ###


<pre><code>
compile_ret() = {ok, binary()} | {error, any()}
</code></pre>




### <a name="type-compiler">compiler()</a> ###


<pre><code>
compiler() = haki_syntax_compiler | haki_beam_compiler | haki_asm_compiler
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cache-2">cache/2</a></td><td>Creates a new module with given Key and stores the Value.</td></tr><tr><td valign="top"><a href="#cache-3">cache/3</a></td><td>Creates a new module with given Key and stores the Value while
forcing the compiler that is used to create the module.</td></tr><tr><td valign="top"><a href="#cache_bucket-2">cache_bucket/2</a></td><td>Creates a new module named after the bucket, each key/value pair of the
given map will be separately retrievable by get/2.</td></tr><tr><td valign="top"><a href="#cache_bucket-3">cache_bucket/3</a></td><td>Creates a new module named after the bucket, each key/value pair of the
given map will be separately retrievable by get/2.</td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td>Retrieves the value for the given Key, by finding the module name
and calling get/0 on it.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>Retrieves the value for the given Key in the given bucket, by finding the module
name and calling get/1 on it.</td></tr><tr><td valign="top"><a href="#load_snapshot-1">load_snapshot/1</a></td><td>Loads a cached key snapshot from the binary file.</td></tr><tr><td valign="top"><a href="#load_snapshot-2">load_snapshot/2</a></td><td>Loads a cached key snapshot from the binary file given a path.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cache-2"></a>

### cache/2 ###

<pre><code>
cache(Key::<a href="#type-cache_key">cache_key()</a>, Val::<a href="#type-cache_value">cache_value()</a>) -&gt; ok | {error, any()}
</code></pre>
<br />

Creates a new module with given Key and stores the Value

<a name="cache-3"></a>

### cache/3 ###

<pre><code>
cache(Key::<a href="#type-cache_key">cache_key()</a>, Val::<a href="#type-cache_value">cache_value()</a>, Options::<a href="#type-cache_options">cache_options()</a>) -&gt; ok | {error, any()}
</code></pre>
<br />

Creates a new module with given Key and stores the Value while
forcing the compiler that is used to create the module.

<a name="cache_bucket-2"></a>

### cache_bucket/2 ###

<pre><code>
cache_bucket(Bucket::<a href="#type-cache_bucket_name">cache_bucket_name()</a>, Map::<a href="#type-cache_bucket_value">cache_bucket_value()</a>) -&gt; ok | {error, any()}
</code></pre>
<br />

Creates a new module named after the bucket, each key/value pair of the
given map will be separately retrievable by get/2.

<a name="cache_bucket-3"></a>

### cache_bucket/3 ###

<pre><code>
cache_bucket(Bucket::<a href="#type-cache_bucket_name">cache_bucket_name()</a>, Map::<a href="#type-cache_bucket_value">cache_bucket_value()</a>, Options::<a href="#type-cache_options">cache_options()</a>) -&gt; ok | {error, any()}
</code></pre>
<br />

Creates a new module named after the bucket, each key/value pair of the
given map will be separately retrievable by get/2.

<a name="get-1"></a>

### get/1 ###

<pre><code>
get(Key::<a href="#type-cache_key">cache_key()</a>) -&gt; <a href="#type-cache_value">cache_value()</a> | bad_key
</code></pre>
<br />

Retrieves the value for the given Key, by finding the module name
and calling get/0 on it.

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Bucket::<a href="#type-cache_bucket_name">cache_bucket_name()</a>, Key::<a href="#type-cache_key">cache_key()</a>) -&gt; <a href="#type-cache_value">cache_value()</a> | bad_key | bad_bucket
</code></pre>
<br />

Retrieves the value for the given Key in the given bucket, by finding the module
name and calling get/1 on it.

<a name="load_snapshot-1"></a>

### load_snapshot/1 ###

<pre><code>
load_snapshot(Key::<a href="#type-cache_key">cache_key()</a>) -&gt; {module, module()} | {error, any()}
</code></pre>
<br />

Loads a cached key snapshot from the binary file.

<a name="load_snapshot-2"></a>

### load_snapshot/2 ###

<pre><code>
load_snapshot(Path::string(), Key::<a href="#type-cache_key">cache_key()</a>) -&gt; {module, module()} | {error, any()}
</code></pre>
<br />

Loads a cached key snapshot from the binary file given a path.

