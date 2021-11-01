%%%-------------------------------------------------------------------
%%% @copyright 2021 Trevor Brown (Apache-2.0 License)
%%% @doc
%%% Generates raw trace messages. This output format is for testing.
%%% @end
%%%-------------------------------------------------------------------
-module(eflambe_plain).

-behaviour(eflambe_output_formatter).

-export([extension/0, init/1, handle_trace_event/2, finalize/2]).

-record(state, {options :: eflambe:options(), data = []}).

extension() -> {ok, <<"txt">>}.

%%--------------------------------------------------------------------
%% @doc
%% This callback exists so the implementation can initialize its own internal
%% state.
%%
%% @end
%%--------------------------------------------------------------------
init(Options) ->
    {ok, #state{options = Options}}.

%%--------------------------------------------------------------------
%% @doc
%% This callback exists so the implementation module can process each individual
%% trace event.
%%
%% @end
%%--------------------------------------------------------------------
handle_trace_event(TraceEvent, #state{data = Data} = State) ->
    Line = io_lib:format("~w~n", [TraceEvent]),
    {ok, State#state{data = [Line|Data]}}.

%%--------------------------------------------------------------------
%% @doc
%% This callback exists so the implementation module can finalize processing of
%% the trace data. For example, any last minute formatting or flushing of data
%% in state to disk.
%%
%% @end
%%--------------------------------------------------------------------
finalize(_Type, #state{data = Data}) ->
    {ok, lists:reverse(Data)}.
