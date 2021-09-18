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

-spec capture(MFA :: mfa(), NumCalls :: integer(), Options :: list()) -> ok.

capture({Module, Function, Arity}, NumCalls, Options) ->
    ok = meck:new(Module, [unstick, passthrough]),


    % Unique identifer for the `NumCalls` number of traces for `Function`
    CaptureRef = make_ref(),

    MockFun = mock_fun(Arity, fun(Args) ->
                                      Trace = start_trace(CaptureRef, NumCalls, Options),

                                      % Invoke the original function
                                      Results = meck:passthrough(Args),

                                      stop_trace(Trace),
                                      Results
                              end),

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

-spec apply(Function :: mfa_fun(), NumCalls :: integer(), Options :: list()) -> any().

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

-spec start_trace(TraceId :: any(), NumCalls :: integer(), Options :: list()) -> ok.

start_trace(TraceId, NumCalls, Options) ->
    % TODO: Implement this function, this function should return some sort of trace ID
    ok.

-spec stop_trace(any()) -> any().

stop_trace(_Trace) ->
    % TODO: Implement this function
    ok.

% Total hack
% TODO: Is there a way to programmatically generate a function of a given arity?
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
