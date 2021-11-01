%%%-------------------------------------------------------------------
%%% @copyright 2021 Trevor Brown (Apache-2.0 License)
%%% @doc
%%% eflambe profiling functions. Invoke these public functions to perform
%%% profiling of functions in your Erlang/Elixir application.
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

-type capture_return() :: {ok, [any()]} | {error, already_mecked}.

-export_type([mfa_fun/0, options/0, capture_return/0]).

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

-spec capture(MFA :: mfa()) -> capture_return().

capture(MFA) ->
    capture(MFA, 1).

-spec capture(MFA :: mfa(), NumCalls :: pos_integer()) -> capture_return().

capture(MFA, NumCalls) ->
    capture(MFA, NumCalls, []).

-spec capture(MFA :: mfa(), NumCalls :: pos_integer(), Options :: options()) ->
    capture_return().

capture({Module, Function, Arity}, NumCalls, Options)
  when is_atom(Module), is_atom(Function), is_integer(Arity) ->
    CompleteOptions = merge([{max_calls, NumCalls}|Options], ?DEFAULT_CAPTURE_OPTIONS),

    setup_for_trace(),

    eflambe_sup:start_trace({Module, Function, Arity}, CompleteOptions).

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
    Return = proplists:get_value(return, CompleteOptions),

    setup_for_trace(),
    {ok, TracePid} = eflambe_server:start_trace(CompleteOptions),

    % Invoke the original function
    Results = erlang:apply(Module, Function, Args),

    {ok, FlameGraph} = eflambe_server:stop_trace(TracePid),

    case Return of
        value -> Results;
        _ -> FlameGraph
    end;

apply({Function, Args}, Options) when is_function(Function), is_list(Args) ->
    CompleteOptions = merge(Options, ?DEFAULT_APPLY_OPTIONS),
    Return = proplists:get_value(return, CompleteOptions),

    setup_for_trace(),
    {ok, TracePid} = eflambe_server:start_trace(CompleteOptions),

    % Invoke the original function
    Results = erlang:apply(Function, Args),

    {ok, FlameGraph} = eflambe_server:stop_trace(TracePid),

    case Return of
        value -> Results;
        _ -> FlameGraph
    end.

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
