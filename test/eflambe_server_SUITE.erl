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
         start_trace/1,
         stop_trace/1
        ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     %% TODO: Group names here e.g. {group, crud}
     start_link,
     start_trace,
     stop_trace
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

groups() ->
    [
        %% TODO: group definitions here e.g.
        %% {crud, [], [t_create_resource, t_read_resource, t_update_resource,
        %%     t_delete_resource]}

    ].

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
    {ok, Pid} = eflambe_server:start_link(),
    true = is_pid(Pid).

start_trace(_Config) ->
    % Returns an error when eflambe_server isn't running
    {'EXIT', {noproc, {gen_server,call,_}}} = (catch eflambe_server:start_trace(foobar, 1, [])),

    % Returns an ok tuple when eflambe_server is running and arguments are valid
    {ok, _Pid} = eflambe_server:start_link(),
    {state,[]} = get_gen_server_state(eflambe_server),
    {ok, foobar} = eflambe_server:start_trace(foobar, 1, []),

    % Stores trace state
    {state,[{trace,foobar,1,0,[]}]} = get_gen_server_state(eflambe_server),

    % Returns end_trace when max number of calls is reached
    {end_trace,foobar,1,[]} = eflambe_server:start_trace(foobar, 1, []),
    {state,[{trace,foobar,1,1,[]}]} = get_gen_server_state(eflambe_server).

stop_trace(_Config) ->
    % Returns an error when eflambe_server isn't running
    {'EXIT', {noproc, {gen_server,call,_}}} = (catch eflambe_server:stop_trace(foobar)),

    % Returns an error tuple when server is running but trace does not exist
    {ok, _Pid} = eflambe_server:start_link(),
    {error, unknown_trace} = eflambe_server:stop_trace(foobar),

    % Returns an ok tuple when eflambe_server is running and arguments are valid
    {ok, foobar} = eflambe_server:start_trace(foobar, 1, []),

    % Updates trace state on the server
    {state,[{trace,foobar,1,0,[]}]} = get_gen_server_state(eflambe_server).

get_gen_server_state(Name) ->
    {status, _, _, State} = sys:get_status(Name),
    [[_, _, {data, [{"State", ServerState}]}]|_] = lists:reverse(State),
    io:format("~p~n", [ServerState]),
    ServerState.

