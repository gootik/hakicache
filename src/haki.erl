%%%-------------------------------------------------------------------
%%% @doc A static cache storage for Erlang.
%%%      Currently consists of two compilers:
%%%         ASM - Used for large lists as value. Read the module docs
%%%               for more info.
%%%
%%%         Syntax - Used for all other cases.
%%%
%%%         Beam - This is experimental and only works for OTP20+
%%% @end
%%%-------------------------------------------------------------------
-module(haki).

-include("internal.hrl").
-include("types.hrl").

-export([
    cache/2,
    cache/3,
    get/1,

    load_snapshot/1
]).

%% @doc Creates a new module with given Key and stores the Value
%% @end
-spec cache(cache_key(), cache_value()) -> ok | {error, any()}.
cache(Key, Val) ->
    cache(Key, Val, ?DEFAULT_CACHE_OPTIONS).

%% @doc Creates a new module with given Key and stores the Value while
%%      forcing the compiler that is used to create the module.
%% @end
-spec cache(cache_key(), cache_value(), cache_options()) -> ok | {error, any()}.
cache(Key, Val, Options) ->
    ?timed(cache,
           begin
               FilledOptions = maps:merge(?DEFAULT_CACHE_OPTIONS, Options),
               haki_compiler:compile(Key, Val, FilledOptions)
           end
    ).

%% @doc Retrieves the value for the given Key, by finding the module name
%%      and calling get/0 on it.
%% @end
-spec get(cache_key()) -> cache_value() | bad_key.
get(Key) ->
    ?timed(get,
           begin
               Mod = haki_compiler:mod_name(Key),
               case module_loaded(Mod) of
                   false ->
                       bad_key;
                   _ ->
                       Mod:get()
               end
           end
    ).

%% @doc Loads a cached key snapshot from the binary file.
%% @end
-spec load_snapshot(cache_key()) -> {module, module()} | {error, any()}.
load_snapshot(Key) ->
    ?timed(load_snapshot,
           begin
               ModName = haki_compiler:mod_name(Key),
               code:load_file(ModName)
           end
    ).