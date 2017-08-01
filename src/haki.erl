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

    cache_bucket/2,
    cache_bucket/3,

    get/1,
    get/2,

    load_snapshot/1,
    load_snapshot/2
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

%% @doc Creates a new module named after the bucket, each key/value pair of the
%%      given map will be separately retrievable by get/2.
%% @end
-spec cache_bucket(cache_bucket_name(), cache_bucket_value()) -> ok | {error, any()}.
cache_bucket(Bucket, Map) ->
    cache_bucket(Bucket, Map, ?DEFAULT_CACHE_OPTIONS).

%% @doc Creates a new module named after the bucket, each key/value pair of the
%%      given map will be separately retrievable by get/2.
%% @end
-spec cache_bucket(cache_bucket_name(), cache_bucket_value(), cache_options()) -> ok | {error, any()}.
cache_bucket(Bucket, Map, Options) ->
    ?timed(cache,
        begin
            FilledOptions = maps:merge(?DEFAULT_CACHE_OPTIONS, Options),
            haki_compiler:compile_bucket(Bucket, Map, FilledOptions)
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

%% @doc Retrieves the value for the given Key in the given bucket, by finding the module
%%      name and calling get/1 on it.
%% @end
-spec get(cache_bucket_name(), cache_key()) -> cache_value() | bad_key | bad_bucket.
get(Bucket, Key) ->
    ?timed(get,
        begin
            Mod = haki_compiler:mod_name(Bucket),
            case module_loaded(Mod) of
                false ->
                    bad_bucket;
                _ ->
                    Mod:get(Key)
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

%% @doc Loads a cached key snapshot from the binary file given a path.
%% @end
-spec load_snapshot(cache_key()) -> {module, module()} | {error, any()}.
load_snapshot(Path, Key) ->
    ?timed(load_snapshot,
           begin
               code:add_pathz(Path),
               ModName = haki_compiler:mod_name(Key),
               code:load_file(ModName)
           end
    ).