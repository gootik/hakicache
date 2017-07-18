%%%-------------------------------------------------------------------
%% @doc haki public API
%% @end
%%%-------------------------------------------------------------------
-module(haki_app).

-behaviour(application).


-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    haki_sup:start_link().

stop(_State) ->
    ok.
