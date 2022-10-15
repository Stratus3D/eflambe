%%%-------------------------------------------------------------------
%%% @doc
%%% Output formatter behaviour. This behavior is used so we can swap in a stub
%%% for testing the tracer.
%%% @end
%%%-------------------------------------------------------------------
-module(eflambe_output_formatter).

%%--------------------------------------------------------------------
%% This callback exists to inform the tracer of the file extension that should
%% be used for the output filename it will generate.
%%
%%--------------------------------------------------------------------
-callback extension() -> {ok, binary()}.

%%--------------------------------------------------------------------
%% This callback exists so the implementation can initialize its own internal
%% state.
%%
%%--------------------------------------------------------------------
-callback init(Options :: eflambe:options()) -> {ok, State :: any()}.

%%--------------------------------------------------------------------
%% This callback exists so the implementation module can process each individual
%% trace event.
%%
%%--------------------------------------------------------------------
-callback handle_trace_event(TraceEvent :: any(), State :: any()) -> {ok, UpdatedState :: any()}.

%%--------------------------------------------------------------------
%% This callback exists so the implementation module can finalize processing of
%% the trace data.
%%
%%--------------------------------------------------------------------
-callback finalize(Type :: return | file, State :: any()) -> {ok, Results :: any()}.
