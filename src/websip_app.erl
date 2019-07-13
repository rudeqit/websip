%%%-------------------------------------------------------------------
%% @doc websip public API
%% @end
%%%-------------------------------------------------------------------

-module(websip_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    websip_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
