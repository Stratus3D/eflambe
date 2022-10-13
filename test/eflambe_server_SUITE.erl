%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(eflambe_server_SUITE).


%% API
-export([all/0,
         suite/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test cases
-export([
         start_link/1,
         start_capture_trace/1,
         stop_capture_trace/1
        ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     start_link,
     start_capture_trace,
     stop_capture_trace
    ].

suite() ->
    [{ct_hooks, [cth_surefire]}, {timetrap, {seconds, 30}}].

groups() ->
    [].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.


%%%===================================================================
%%% Group specific setup/teardown
%%%===================================================================
group(_Groupname) ->
    [].

init_per_group(_Groupname, Config) ->
    Config.

end_per_group(_Groupname, _Config) ->
    ok.


%%%===================================================================
%%% Testcase specific setup/teardown
%%%===================================================================
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================

start_link(_Config) ->
    {ok, Pid} = eflambe_server:start_link({arithmetic, multiply, 2}, [{max_calls, 1}]),
    true = is_pid(Pid),

    eflambe_server:stop_capture_trace(Pid).

start_capture_trace(_Config) ->
    Options = [{max_calls, 1}, {output_format, plain}],
    Self = self(),

    % Returns an ok tuple when eflambe_server is running and arguments are valid
    {ok, Pid} = eflambe_server:start_link({arithmetic, multiply, 2}, Options),
    {state,arithmetic,1,0, FinalOptions, []} = get_server_state(Pid),

    {ok, true} = eflambe_server:start_capture_trace(Pid),

    % Stores trace state
    {state, arithmetic, 1, 1, FinalOptions, [{pid_trace, Self, _Tracer}]} = State =
      get_server_state(Pid),

    % Returns the same trace data when called twice
    {ok, false} = eflambe_server:start_capture_trace(Pid),
    State = get_server_state(Pid),

    % Returns false when trace is stopped but number of calls has already been reached
    ok = eflambe_server:stop_capture_trace(Pid).

stop_capture_trace(_Config) ->
    Options = [{max_calls, 2}, {output_format, plain}],
    Self = self(),

    {ok, Pid} = eflambe_server:start_link({arithmetic, multiply, 2}, Options),

    % Returns an ok tuple when eflambe_server is running and arguments are valid
    {ok, true} = eflambe_server:start_capture_trace(Pid),

    % Stores trace state
    {state, arithmetic, 2, 1, FinalOptions, [{pid_trace, Self, _}]} = get_server_state(Pid),

    % When called removes trace from server state
    ok = eflambe_server:stop_capture_trace(Pid),
    {state, arithmetic, 2, 1, FinalOptions, []} = get_server_state(Pid),

    % After being called start_capture_trace invoked in the same process returns
    % true to kick off a new trace
    {ok, true} = eflambe_server:start_capture_trace(Pid),
    ok = eflambe_server:stop_capture_trace(Pid).

get_server_state(Pid) ->
    {status, _, _, State} = sys:get_status(Pid),
    [[_, _, {data, [{"State", ServerState}]}]|_] = lists:reverse(State),
    ServerState.
