%%%-------------------------------------------------------------------
%% @doc estatic public API
%% @end
%%%-------------------------------------------------------------------
-module(estatic_app).

-behaviour(application).


-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    estatic_sup:start_link().

stop(_State) ->
    ok.
