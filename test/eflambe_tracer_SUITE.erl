%%%-------------------------------------------------------------------
%%% @author trevor
%%% @copyright (C) 2021, trevor
%%% @doc
%%%
%%% @end
%%% Created : 2021-09-17 20:11:17.715138
%%%-------------------------------------------------------------------
-module(eflambe_tracer_SUITE).


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
         start_link/1
        ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     %% TODO: Group names here e.g. {group, crud}
     start_link
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
    {ok, Pid} = eflambe_tracer:start_link(),
    true = is_pid(Pid).
