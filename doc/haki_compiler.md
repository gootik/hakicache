

# Module haki_compiler #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

The compiler behavior and module that controls the compiling
of the cache key/val.

__This module defines the `haki_compiler` behaviour.__<br /> Required callback functions: `compile/2`.

<a name="types"></a>

## Data Types ##




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
cache_options() = #{compiler =&gt; <a href="#type-compiler">compiler()</a> | haki_default_compiler, save_binary =&gt; boolean()}
</code></pre>




### <a name="type-cache_value">cache_value()</a> ###


<pre><code>
cache_value() = any()
</code></pre>




### <a name="type-compile_ret">compile_ret()</a> ###


<pre><code>
compile_ret() = ok | {error, any()}
</code></pre>




### <a name="type-compiler">compiler()</a> ###


<pre><code>
compiler() = haki_syntax_compiler | haki_beam_compiler | haki_asm_compiler
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#compile-3">compile/3</a></td><td></td></tr><tr><td valign="top"><a href="#mod_name-1">mod_name/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="compile-3"></a>

### compile/3 ###

<pre><code>
compile(Key::<a href="#type-cache_key">cache_key()</a>, Val::<a href="#type-cache_value">cache_value()</a>, Options::<a href="#type-cache_options">cache_options()</a>) -&gt; <a href="#type-compile_ret">compile_ret()</a>
</code></pre>
<br />

<a name="mod_name-1"></a>

### mod_name/1 ###

<pre><code>
mod_name(Key::<a href="#type-cache_key">cache_key()</a>) -&gt; atom()
</code></pre>
<br />

