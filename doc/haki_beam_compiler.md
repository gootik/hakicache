

# Module haki_beam_compiler #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

NOTE: Super hacky (but fast) at the moment.

<a name="description"></a>

## Description ##
If you want to use
this compiler you have to force it by forcing it. For example:
haki:cache(K, V, haki_beam_compiler).

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
compile_ret() = {ok, binary()} | {error, any()}
</code></pre>




### <a name="type-compiler">compiler()</a> ###


<pre><code>
compiler() = haki_syntax_compiler | haki_beam_compiler | haki_asm_compiler
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#compile-2">compile/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="compile-2"></a>

### compile/2 ###

<pre><code>
compile(ModName::<a href="#type-cache_module_name">cache_module_name()</a>, Val::<a href="#type-cache_value">cache_value()</a>) -&gt; <a href="#type-compile_ret">compile_ret()</a>
</code></pre>
<br />

