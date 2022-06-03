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

-type mfa_fun() :: {atom(), atom(), list()} | fun().

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
    ok = meck:new(Module, [unstick, passthrough]),
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

    MockFun = gen_mock_fun(Arity, ShimmedFunction),

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


% This function dynamically generates a function of a specified arity that
% invokes `Function` with the list of all the arguments.
% https://stackoverflow.com/questions/69244814/erlang-generate-anonymous-function-of-an-arbitary-arity

-spec gen_mock_fun(non_neg_integer(), function()) -> function().

gen_mock_fun(Arity, Function) when is_function(Function) ->
    ParamVars = [list_to_atom([$X| integer_to_list(I)]) || I <- lists:seq(1, Arity)],
    Params = [{var, 1, Var} || Var <- ParamVars],
    ParamsList = lists:foldl(fun(Elem, Acc) ->
                        {cons, 1, {var, 1, Elem}, Acc}
                end, {nil, 1}, lists:reverse(ParamVars)),

    Anno = erl_anno:new(1),

    FunctionCall = {call, Anno, {var, Anno, 'Function'}, [ParamsList]},
    Expr =
        {'fun',
         Anno,
         {clauses, [{clause, Anno, Params, [], [FunctionCall]}]}},

    {value, Fun, _Vars} = erl_eval:expr(Expr, [{'Function', Function}]),
    Fun.
