%%%-------------------------------------------------------------------
%%% @author trevor
%%% @copyright (C) 2021, trevor
%%% @doc
%%%
%%% @end
%%% Created : 2021-09-17 21:07:41.807632
%%%-------------------------------------------------------------------
-module(eflambe_SUITE).


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
         apply/1,
         capture/1
        ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     apply,
     capture
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

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

capture(_Config) ->
    Options = [],

    % Shouldn't crash when invoked
    eflambe:capture({arithmetic, multiply, 2}, 1, Options),

    12 = arithmetic:multiply(4,3),

    % Should behave the same when run a second time
    eflambe:capture({arithmetic, multiply, 2}, 1, Options),

    12 = arithmetic:multiply(4,3),

    ok = application:stop(eflambe).

apply(_Config) ->
    Options = [],

    % Shouldn't crash when invoked
    eflambe:apply({arithmetic, multiply, [2,3]}, 1, Options),

    % Should behave the same when run a second time
    eflambe:apply({arithmetic, multiply, [2,3]}, 1, Options),

    ok = application:stop(eflambe).
