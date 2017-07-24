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
    cache_bucket/2,
    get/1,
    get/2,

    snapshot/1,

    t/0,
    ts/0
]).

-spec cache(cache_key(), cache_value()) -> ok | {error, any()}.
cache(Key, Val) ->
    ?timed(cache,
           haki_compiler:compile(Key, Val)
    ).

cache_bucket(Bucket, Map) ->
    ?timed(cache,
       haki_compiler:compile_bucket(Bucket, Map)
    ).

-spec get(cache_key()) -> cache_value().
get(Key) ->
    ?timed(get,
           begin
               Mod = haki_compiler:mod_name(Key),
               Mod:get()
           end
    ).

get(Bucket, Key) ->
    ?timed(get,
           begin
               Mod = haki_compiler:mod_name(Bucket, Key),
               Mod:get(Key)
           end
    ).

snapshot(Key) ->
    Mod = haki_compiler:mod_name(Key),
    Mod.

t() ->
    ?timed(test,
           begin
               {ok, TabId} = ets:file2tab("./sample.ets"),
               [{_, Data}] = ets:lookup(TabId, <<"phone">>),

               haki:cache(test_key, Data),
               Data = haki:get(test_key),

               ok
           end).

ts() ->
    ?timed(test,
           begin
               {ok, TabId} = ets:file2tab("./sample.ets"),
               [{_, Data}] = ets:lookup(TabId, <<"phone">>),

               CacheData = lists:sublist(Data, 2),
               haki:cache(random_key, CacheData),
               CacheData = haki:get(random_key),

               ok
           end).