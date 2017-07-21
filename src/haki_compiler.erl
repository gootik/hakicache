%%%-------------------------------------------------------------------
%%% @doc The compiler behavior and module that controls the compiling
%%%      of the cache key/val.
%%% @end
%%%-------------------------------------------------------------------
-module(haki_compiler).

-include("internal.hrl").
-include("types.hrl").

-export([
    compile/2,
    compile_bucket/2,

    mod_name/1
]).


-callback compile(cache_module_name(), cache_value()) -> compile_ret().

-spec compile(cache_key(), cache_value()) -> compile_ret().
compile(Key, Val) when is_list(Val) andalso length(Val) > ?LARGE_LIST_LENGTH ->
    ModName = mod_name(Key),
    haki_asm_compiler:compile(ModName, Val);

compile(Key, Val) ->
    ModName = mod_name(Key),
    haki_syntax_compiler:compile(ModName, Val).

compile_bucket(Bucket, Map) when map_size(Map) > ?LARGE_MAP_SIZE ->
    ModName = mod_name(Bucket),
    haki_asm_compiler:compile_bucket(ModName, Map);

compile_bucket(Bucket, Map) ->
    ModName = mod_name(Bucket),
    haki_syntax_compiler:compile_bucket(ModName, Map).

mod_name(Key) ->
    list_to_atom("haki_" ++ atom_to_list(Key)).