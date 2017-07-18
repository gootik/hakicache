%%%-------------------------------------------------------------------
%%% @doc A static cache storage for Erlang.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(estatic).

-include("internal.hrl").
-include("types.hrl").

-export([
    cache/2,
    get/1
]).

-export([
    t/0,
    t2/0
]).

-spec cache(cache_key(), cache_value()) -> ok | {error, any()}.
cache(Key, Val) ->
    ?timed(cache,
        estatic_compiler:compile(Key, Val)
    ).

-spec get(cache_key()) -> cache_value().
get(Key) ->
    ?timed(get,
        begin
            Mod = estatic_compiler:mod_name(Key),
            Mod:get()
        end
    ).

t() ->
    ?timed(test,
        begin
            {ok, TabId} = ets:file2tab("./sample.ets"),
            [{_, Data}] = ets:lookup(TabId, <<"phone">>),

            CacheData = lists:sublist(Data, 5000),
            estatic:cache(test_key, CacheData),
            CacheData = estatic:get(test_key),

            ok
        end).

t2() ->
    ?timed(test,
       begin
           {ok, TabId} = ets:file2tab("./sample.ets"),
           [{_, Data}] = ets:lookup(TabId, <<"phone">>),

           CacheData = lists:sublist(Data, 2),
           estatic:cache(test_key, CacheData),
           CacheData = estatic:get(test_key),

           ok
       end).