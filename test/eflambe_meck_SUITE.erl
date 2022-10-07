%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(eflambe_meck_SUITE).


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
         shim/1,
         unload/1
        ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     shim,
     unload
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

shim(_Config) ->
    ShimFun = fun([X, Y]) ->
                      meck:passthrough([X, Y])
              end,

    % It succeeds when passed valid arguments
    ok = eflambe_meck:shim(arithmetic, divide, 2, ShimFun),

    % It raise an exit when passed a module that has already been mocked
    {'EXIT', {{already_started,_}, _}} = (catch eflambe_meck:shim(arithmetic, divide, 2, ShimFun)),

    meck:unload(arithmetic).

unload(_Config) ->
    % It throws an error when module is not mocked
    {'EXIT', {{not_mocked, arithmetic}, _}} = (catch eflambe_meck:unload(arithmetic)),

    ShimFun = fun([X, Y]) ->
                      meck:passthrough([X, Y])
              end,
    ok = eflambe_meck:shim(arithmetic, divide, 2, ShimFun),

    % It returns ok if function is mocked with meck
    ok = eflambe_meck:unload(arithmetic).
