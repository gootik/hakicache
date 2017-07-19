

# Module haki_asm_compiler #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Using the syntax compiler has an over head of creating an AST
and having the compiler to lint/expand records/optimize.

<a name="description"></a>

## Description ##

Moreover, building an AST and compiling has a huge overhead on
memory, if the data is large.

However for the most usecases of this library you are caching
a literal value, so if we hack the ASM file on the fly it should
produce a decent module that can be then compiled and loaded.
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




### <a name="type-cache_value">cache_value()</a> ###


<pre><code>
cache_value() = any()
</code></pre>




### <a name="type-compile_ret">compile_ret()</a> ###


<pre><code>
compile_ret() = ok | {error, any()}
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

