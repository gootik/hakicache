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
    compile/3,

    mod_name/1
]).

-define(NUM_BUCKETS_MODS, 10).

-callback compile(cache_module_name(), cache_value()) -> compile_ret().

-spec compile(cache_key(), cache_value()) -> compile_ret().
compile(Key, Val) when is_list(Val) andalso length(Val) > ?LARGE_LIST_LENGTH ->
    ModName = mod_name(Key),
    haki_beam_compiler:compile(ModName, Val);

compile(Key, Val) ->
    ModName = mod_name(Key),
    haki_syntax_compiler:compile(ModName, Val).

compile(Key, Val, Compiler) ->
    ModName = mod_name(Key),
    Compiler:compile(ModName, Val).

-spec mod_name(cache_key()) -> atom().
mod_name(Key) ->
    list_to_atom("haki_" ++ atom_to_list(Key)).