

# Module haki_syntax_compiler #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Using the simple erl_syntax module to build a AST before
compiling the module.

<a name="description"></a>

## Description ##
This is way more stable than the ASM
compiler and thus should be used for most cases.
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#compile-3">compile/3</a></td><td></td></tr><tr><td valign="top"><a href="#compile_bucket-3">compile_bucket/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="compile-3"></a>

### compile/3 ###

<pre><code>
compile(ModName::<a href="#type-cache_module_name">cache_module_name()</a>, Val::<a href="#type-cache_value">cache_value()</a>, Options::<a href="#type-cache_options">cache_options()</a>) -&gt; <a href="#type-compile_ret">compile_ret()</a>
</code></pre>
<br />

<a name="compile_bucket-3"></a>

### compile_bucket/3 ###

<pre><code>
compile_bucket(ModName::<a href="#type-cache_module_name">cache_module_name()</a>, Map::<a href="#type-cache_bucket_value">cache_bucket_value()</a>, Options::<a href="#type-cache_options">cache_options()</a>) -&gt; <a href="#type-compile_ret">compile_ret()</a>
</code></pre>
<br />

