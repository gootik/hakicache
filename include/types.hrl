-type cache_options() :: #{
    compiler => compiler() | haki_default_compiler,
    save_snapshot => boolean()
}.

-type cache_module_name() :: atom().
-type cache_key() :: atom().
-type cache_value() :: any().

-type cache_bucket_name() :: atom().
-type cache_bucket_value() :: #{
    cache_key() => cache_value()
}.

-type compile_ret() :: {ok, binary()} | {error, any()}.

-type compiler() :: haki_syntax_compiler |
                    haki_beam_compiler |
                    haki_asm_compiler.

-export_type([
    cache_module_name/0,

    cache_key/0,
    cache_value/0,

    cache_bucket_name/0,
    cache_bucket_value/0,

    cache_options/0,

    compile_ret/0,

    compiler/0
]).