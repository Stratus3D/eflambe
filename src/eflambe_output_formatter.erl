%%%-------------------------------------------------------------------
%%% @doc
%%% Output formatter behaviour. This behavior is used so we can swap in a stub
%%% for testing the tracer.
%%% @end
%%%-------------------------------------------------------------------
-module(eflambe_output_formatter).

%%--------------------------------------------------------------------
%% @doc
%% This callback exists so the implementation can initialize its own internal
%% state. This may be useful for opening files, etc...
%%
%% @end
%%--------------------------------------------------------------------
-callback init(Filename :: binary(), Options :: eflambe:options()) -> {ok, State :: any()}.

%%--------------------------------------------------------------------
%% @doc
%% This callback exists so the implementation module can process each individual
%% trace event.
%%
%% @end
%%--------------------------------------------------------------------
-callback handle_trace_event(TraceEvent :: any(), State :: any()) -> {ok, UpdatedState :: any()}.

%%--------------------------------------------------------------------
%% @doc
%% This callback exists so the implementation module can finalize processing of
%% the trace data. For example, any last minute formatting or flushing of data
%% in state to disk.
%%
%% @end
%%--------------------------------------------------------------------
-callback finalize(Options :: eflambe:options(), State :: any()) -> {ok, State :: any()}.
