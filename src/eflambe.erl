%%%-------------------------------------------------------------------
%%% @doc
%%% This module provides the public API for eflambe. These public functions are
%%% intended to be invoked by the end user to perform profiling of their
%%% application.
%%% @end
%%%-------------------------------------------------------------------
-module(eflambe).

%% Application callbacks
-export([capture/1, capture/2, capture/3,
         apply/1, apply/2]).

-type mfa_fun() :: {atom(), atom(), list()} | {fun(), list()}.

-type program() :: hotspot | speedscope.
-type option() :: {output_directory, binary()} | {output_format, brendan_gregg} | {open, program()}.
-type options() :: [option()].

-define(FLAGS, [call, return_to, running, procs, garbage_collection, arity,
                timestamp, set_on_spawn]).

%%--------------------------------------------------------------------
%% @doc
%% Starts capturing of function call data for any invocation of the specified
%% MFA and of a flamegraph for
%% the current process.
%%
%% @end
%%--------------------------------------------------------------------

-spec capture(MFA :: mfa()) -> ok.

capture(MFA) ->
    capture(MFA, 1).

-spec capture(MFA :: mfa(), NumCalls :: pos_integer()) -> ok.

capture(MFA, NumCalls) ->
    capture(MFA, NumCalls, []).

-spec capture(MFA :: mfa(), NumCalls :: pos_integer(), Options :: options()) -> ok.

capture({Module, Function, Arity}, NumCalls, Options)
  when is_atom(Module), is_atom(Function), is_integer(Arity) ->
    TraceId = setup_for_trace(),

    ShimmedFunction = fun(Args) ->
        {Trace, StartedNew} = start_trace(TraceId, NumCalls, [{meck, Module}|Options]),
        % Invoke the original function
        Results = meck:passthrough(Args),

        case StartedNew of
            true ->
                stop_trace(Trace);
            false ->
                ok
        end,
        Results
    end,

    eflambe_meck:shim(Module, Function, Arity, ShimmedFunction).


%%--------------------------------------------------------------------
%% @doc
%% Traces the execution of the function passed in for generation of a for a
%% flamegraph of the function call.
%%
%% @end
%%--------------------------------------------------------------------

-spec apply(Function :: mfa_fun()) -> any().

apply(Function) ->
    ?MODULE:apply(Function, []).

-spec apply(Function :: mfa_fun(), Options :: options()) -> any().

apply({Module, Function, Args}, Options) when is_atom(Module), is_atom(Function), is_list(Args) ->
    TraceId = setup_for_trace(),
    {Trace, _StartedNew} = start_trace(TraceId, 1, Options),

    % Invoke the original function
    Results = erlang:apply(Module, Function, Args),

    stop_trace(Trace),
    Results;

apply({Function, Args}, Options) when is_function(Function), is_list(Args) ->
    TraceId = setup_for_trace(),
    {Trace, _StartedNew} = start_trace(TraceId, 1, Options),

    % Invoke the original function
    Results = erlang:apply(Function, Args),

    stop_trace(Trace),
    Results.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec start_trace(TraceId :: any(), NumCalls :: pos_integer(), Options :: list()) ->
    {reference(), boolean()}.

start_trace(TraceId, NumCalls, Options) ->
    case eflambe_server:start_trace(TraceId, NumCalls, Options) of
        {ok, TraceId, true, Tracer} ->
            MatchSpec = [{'_', [], [{message, {{cp, {caller}}}}]}],
            erlang:trace_pattern(on_load, MatchSpec, [local]),
            erlang:trace_pattern({'_', '_', '_'}, MatchSpec, [local]),
            erlang:trace(self(), true, [{tracer, Tracer} | ?FLAGS]),
            {TraceId, true};
        {ok, TraceId, false, _Tracer} ->
            % Trace is already running or has already finished. Or this could
            % be a recursive function call.  We do not need to do anything.
            {TraceId, false}
    end.

-spec stop_trace(reference()) -> ok.

stop_trace(Trace) ->
    erlang:trace(self(), false, [all]),
    {ok, _} = eflambe_server:stop_trace(Trace),
    ok.

setup_for_trace() ->
    application:ensure_all_started(eflambe),
    eflambe_sup:get_or_start_server(),

    % All traces must have a unique ref so we can keep track of them
    make_ref().
