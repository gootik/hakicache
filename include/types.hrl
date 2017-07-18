-type cache_module_name() :: atom().
-type cache_key() :: atom().
-type cache_value() :: any().

-type compile_ret() :: ok | {error, any()}.

-export_type([
    cache_module_name/0,
    cache_key/0,
    cache_value/0,

    compile_ret/0
]).