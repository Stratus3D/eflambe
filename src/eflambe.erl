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
-type option() :: {return, value | flamegraph | filename}
                | {output_directory, binary()}
                | {output_format, brendan_gregg}
                | {open, program()}.
-type options() :: [option()].

-define(DEFAULT_OPTIONS, [{output_format, brendan_gregg}]).
-define(DEFAULT_APPLY_OPTIONS, [{return, value}|?DEFAULT_OPTIONS]).
-define(DEFAULT_CAPTURE_OPTIONS, [{return, filename}|?DEFAULT_OPTIONS]).

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

-spec capture(MFA :: mfa(), NumCalls :: pos_integer(), Options :: options()) ->
    {ok, binary() | [any()]}  | {error, already_mecked}.

capture({Module, Function, Arity}, NumCalls, Options)
  when is_atom(Module), is_atom(Function), is_integer(Arity) ->
    CompleteOptions = merge([{max_calls, NumCalls}|Options], ?DEFAULT_CAPTURE_OPTIONS),

    setup_for_trace(),

    case eflambe_sup:start_trace({Module, Function, Arity}, CompleteOptions) of
        {ok, _Pid} -> ok;
        {error, _} = Error -> Error
    end.

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
    CompleteOptions = merge(Options, ?DEFAULT_APPLY_OPTIONS),

    setup_for_trace(),
    {ok, TracePid} = eflambe_server:start_trace(CompleteOptions),

    % Invoke the original function
    Results = erlang:apply(Module, Function, Args),

    ok = eflambe_server:stop_trace(TracePid),
    Results;

apply({Function, Args}, Options) when is_function(Function), is_list(Args) ->
    CompleteOptions = merge(Options, ?DEFAULT_APPLY_OPTIONS),

    setup_for_trace(),
    {ok, TracePid} = eflambe_server:start_trace(CompleteOptions),

    % Invoke the original function
    Results = erlang:apply(Function, Args),

    ok = eflambe_server:stop_trace(TracePid),
    Results.

%%%===================================================================
%%% Internal functions
%%%===================================================================

setup_for_trace() ->
    application:ensure_all_started(eflambe).

% https://stackoverflow.com/questions/21873644/combine-merge-two-erlang-lists
merge(In1, In2) ->
    Combined = In1 ++ In2,
    Fun = fun(Key) ->
                  [FinalValue|_] = proplists:get_all_values(Key, Combined),
                  {Key, FinalValue}
          end,
    lists:map(Fun, proplists:get_keys(Combined)).
