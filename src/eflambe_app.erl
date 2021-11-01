%%%-------------------------------------------------------------------
%% @doc eflambe public API
%% @end
%% @hidden
%%%-------------------------------------------------------------------

-module(eflambe_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    eflambe_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
