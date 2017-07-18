%%%-------------------------------------------------------------------
%%% @doc A static cache storage for Erlang.
%%%      Currently consists of two compilers:
%%%         ASM - Used for large lists as value. Read the module docs
%%%               for more info.
%%%         Syntax - Used for all other cases.
%%% @end
%%%-------------------------------------------------------------------
-module(haki).

-include("internal.hrl").
-include("types.hrl").

-export([
    cache/2,
    get/1
]).

-spec cache(cache_key(), cache_value()) -> ok | {error, any()}.
cache(Key, Val) ->
    ?timed(cache,
        haki_compiler:compile(Key, Val)
    ).

-spec get(cache_key()) -> cache_value().
get(Key) ->
    ?timed(get,
        begin
            Mod = haki_compiler:mod_name(Key),
            Mod:get()
        end
    ).