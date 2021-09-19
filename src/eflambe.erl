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
         apply/1, apply/2, apply/3]).

-type mfa_fun() :: {atom(), atom(), list()} | fun().

-type option() :: {output_directory, binary()} | {output_format, binary()} | {open, atom()}.
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

-spec capture(MFA :: mfa(), NumCalls :: integer()) -> ok.

capture(MFA, NumCalls) ->
    capture(MFA, NumCalls, []).

-spec capture(MFA :: mfa(), NumCalls :: integer(), Options :: options()) -> ok.

capture({Module, Function, Arity}, NumCalls, Options) ->
    ok = meck:new(Module, [unstick, passthrough]),


    % Unique identifer for the `NumCalls` number of traces for `Function`
    CaptureRef = make_ref(),

    ShimmedFunction = fun(Args) ->
        Trace = start_trace(CaptureRef, NumCalls, [{meck, Module}|Options]),

        % Invoke the original function
        Results = meck:passthrough(Args),

        stop_trace(Trace),
        Results
    end,

    MockFun = mock_fun(Arity, ShimmedFunction),

    % Replace the original function with our new function that wraps the old
    % function in profiling code.
    meck:expect(Module, Function, MockFun).


%%--------------------------------------------------------------------
%% @doc
%% Traces the execution of the function passed in for generation of a for a
%% flamegraph of the function call.
%%
%% @end
%%--------------------------------------------------------------------

-spec apply(Function :: mfa_fun()) -> any().

apply(Function) ->
    ?MODULE:apply(Function, 1).

-spec apply(Function :: mfa_fun(), NumCalls :: integer()) -> any().

apply(Function, NumCalls) ->
    ?MODULE:apply(Function, NumCalls, []).

-spec apply(Function :: mfa_fun(), NumCalls :: integer(), Options :: options()) -> any().

apply({Module, Function, Args}, NumCalls, Options) ->
    TraceId = make_ref(),
    Trace = start_trace(TraceId, NumCalls, Options),

    % Invoke the original function
    Results = erlang:apply(Module, Function, Args),

    stop_trace(Trace),
    Results;

apply({Function, Args}, NumCalls, Options) ->
    TraceId = make_ref(),
    Trace = start_trace(TraceId, NumCalls, Options),

    % Invoke the original function
    Results = erlang:apply(Function, Args),

    stop_trace(Trace),
    Results.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec start_trace(TraceId :: any(), NumCalls :: integer(), Options :: list()) -> reference().

start_trace(TraceId, NumCalls, Options) ->
    % TODO: Figure if I can move this application start code elsewhere
    application:ensure_all_started(eflambe),
    eflambe_sup:get_or_start_server(),

    case eflambe_server:start_trace(TraceId, NumCalls, Options) of
        {ok, TraceId, true, Tracer} ->
            % TODO: Clean up this code
            MatchSpec = [{'_', [], [{message, {{cp, {caller}}}}]}],
            erlang:trace_pattern(on_load, MatchSpec, [local]),
            erlang:trace_pattern({'_', '_', '_'}, MatchSpec, [local]),
            erlang:trace(self(), true, [{tracer, Tracer} | ?FLAGS]);
        {ok, TraceId, false, _Tracer} ->
            % Trace is already running or has already finished. Or this could
            % be a recursive function call.  We do not need to do anything.
            ok
    end,

    TraceId.

-spec stop_trace(any()) -> ok.

stop_trace(Trace) ->
    case eflambe_server:stop_trace(Trace) of
        {error,  unknown_trace} -> ok;
        {ok, _Id, _Calls, false, _Options} -> ok;
        {ok, _Id, _Calls, true, _Options} ->
            % Stop trace
            % TODO: Improve boolean values returned from eflambe_server
            erlang:trace(self(), false, [all])
    end.

% Total hack
% TODO: Is there a way to programmatically generate a function of a given arity?
% I asked here:
% https://stackoverflow.com/questions/69244814/erlang-generate-anonymous-function-of-an-arbitary-arity
mock_fun(1, Function) ->
    fun(A) -> Function([A]) end;
mock_fun(2, Function) ->
    fun(A, B) -> Function([A, B]) end;
mock_fun(3, Function) ->
    fun(A, B, C) -> Function([A, B, C]) end;
mock_fun(4, Function) ->
    fun(A, B, C, D) -> Function([A, B, C, D]) end;
mock_fun(5, Function) ->
    fun(A, B, C, D, E) -> Function([A, B, C, D, E]) end;
mock_fun(6, Function) ->
    fun(A, B, C, D, E, F) -> Function([A, B, C, D, E, F]) end;
mock_fun(7, Function) ->
    fun(A, B, C, D, E, F, G) -> Function([A, B, C, D, E, F, G]) end;
mock_fun(8, Function) ->
    fun(A, B, C, D, E, F, G, H) -> Function([A, B, C, D, E, F, G, H]) end;
mock_fun(9, Function) ->
    fun(A, B, C, D, E, F, G, H, I) -> Function([A, B, C, D, E, F, G, H, I]) end;
mock_fun(10, Function) ->
    fun(A, B, C, D, E, F, G, H, I, J) -> Function([A, B, C, D, E, F, G, H, I, J]) end.
