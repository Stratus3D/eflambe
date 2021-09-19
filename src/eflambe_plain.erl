%%%-------------------------------------------------------------------
%%% @doc
%%% Writes raw trace messages to file. This output format is for testing.
%%% @end
%%%-------------------------------------------------------------------
-module(eflambe_plain).

-behaviour(eflambe_output_formatter).

-export([extension/0, init/2, handle_trace_event/2, finalize/2]).

-record(state, {file :: any(), options :: eflambe:options()}).

extension() -> {ok, <<"txt">>}.

%%--------------------------------------------------------------------
%% @doc
%% This callback exists so the implementation can initialize its own internal
%% state. This may be useful for opening files, etc...
%%
%% @end
%%--------------------------------------------------------------------
init(Filename, Options) ->
    {ok, File} = file:open(Filename, [write, exclusive]),
    {ok, #state{file = File, options = Options}}.

%%--------------------------------------------------------------------
%% @doc
%% This callback exists so the implementation module can process each individual
%% trace event.
%%
%% @end
%%--------------------------------------------------------------------
handle_trace_event(TraceEvent, #state{file = File} = State) ->
    ok = file:write(File, io_lib:format("~w~n", [TraceEvent])),
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% This callback exists so the implementation module can finalize processing of
%% the trace data. For example, any last minute formatting or flushing of data
%% in state to disk.
%%
%% @end
%%--------------------------------------------------------------------
finalize(_Options, #state{file = File} = State) ->
    ok = file:close(File),
    {ok, State}.
