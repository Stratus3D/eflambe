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

    eflambe_server:stop_capture_trace(Pid, return_value).

start_capture_trace(_Config) ->
    Self = self(),
    Ref = make_ref(),
    Options = [{callback, {Self, Ref}}, {max_calls, 1}, {output_format, plain}, {return, filename}],

    % Returns an ok tuple when eflambe_server is running and arguments are valid
    {ok, Pid} = eflambe_server:start_link({arithmetic, multiply, 2}, Options),
    {state, {Self, Ref}, arithmetic, 1, 0, FinalOptions, [], [], filename} = get_server_state(Pid),

    {ok, true} = eflambe_server:start_capture_trace(Pid),

    % Stores trace state
    State = get_server_state(Pid),
    {state, {Self, Ref},  arithmetic, 1, 1, FinalOptions, Traces, [], filename} = State,
    [{pid_trace, Self, _Tracer}] = Traces,

    % Returns the same trace data when called twice
    {ok, false} = eflambe_server:start_capture_trace(Pid),
    State = get_server_state(Pid),

    % Returns false when trace is stopped but number of calls has already been reached
    ok = eflambe_server:stop_capture_trace(Pid, return_value).

stop_capture_trace(_Config) ->
    Self = self(),
    Ref = make_ref(),
    Reply = {Self, Ref},
    Options = [{callback, Reply}, {max_calls, 2}, {output_format, plain}, {return, filename}],

    {ok, Pid} = eflambe_server:start_link({arithmetic, multiply, 2}, Options),

    % Returns an ok tuple when eflambe_server is running and arguments are valid
    {ok, true} = eflambe_server:start_capture_trace(Pid),

    % Stores trace state
    {state, Reply, arithmetic, 2, 1, FinalOptions, Traces, [], filename} = get_server_state(Pid),
    [{pid_trace, Self, _}] = Traces,

    % When called removes trace from server state
    ok = eflambe_server:stop_capture_trace(Pid, return_value),
    State = get_server_state(Pid),
    {state, Reply, arithmetic, 2, 1, FinalOptions, [], [_Filename], filename} = State,

    % After being called start_capture_trace invoked in the same process returns
    % true to kick off a new trace
    {ok, true} = eflambe_server:start_capture_trace(Pid),
    ok = eflambe_server:stop_capture_trace(Pid, return_value).

get_server_state(Pid) ->
    {status, _, _, State} = sys:get_status(Pid),
    [[_, _, {data, [{"State", ServerState}]}]|_] = lists:reverse(State),
    ServerState.
