%%%-------------------------------------------------------------------
%%% @doc
%%% The tests in this file are basic integration tests for the whole library.
%%% They aren't particularly assertive but they allow me to verify some of the
%%% happy paths in the code.
%%% @end
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
         nested_apply/1,
         capture/1,
         capture_same_module/1,
         capture_and_apply_brendan_gregg/1,
         multiple_captures/1
        ]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     apply,
     nested_apply,
     capture,
     capture_same_module,
     capture_and_apply_brendan_gregg,
     multiple_captures
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

capture(_Config) ->
    Options = [{output_format, plain}, {return, filename}],
    MFA = {arithmetic, multiply, 2},

    spawn(fun() ->
                  timer:sleep(100),
                  12 = arithmetic:multiply(4, 3)
               end),

    % Shouldn't crash when invoked
    {ok, [_Filename]} = eflambe:capture(MFA, 1, Options),

    spawn(fun() ->
                  timer:sleep(100),
                  12 = arithmetic:multiply(4, 3)
               end),

    % Should behave the same when run a second time
    {ok, [_Filename2]} = eflambe:capture(MFA, 1, Options),

    ok = application:stop(eflambe).

apply(_Config) ->
    Options = [{output_format, plain}],

    % Shouldn't crash when invoked
    6 = eflambe:apply({arithmetic, multiply, [2, 3]}, Options),

    % Should behave the same when run a second time
    9 = eflambe:apply({arithmetic, multiply, [3, 3]}, Options),

    ok = application:stop(eflambe).

nested_apply(_Config) ->
    Options = [{output_format, plain}],

    % Shouldn't crash when invoked, and returns functions return value
    8 = eflambe:apply({fun() ->
                               arithmetic:multiply(2, 4)
                       end, []}, Options),

    % Should behave the same when run a second time
    12 = eflambe:apply({fun() ->
                               arithmetic:multiply(3, 4)
                       end, []}, Options),

    ok = application:stop(eflambe).


capture_and_apply_brendan_gregg(_Config) ->
    Options = [{output_format, brendan_gregg}],
    % Count files in dir
    {ok, Files} = file:list_dir("."),
    NumFiles = length(Files),

    % Both calls should work with the brendan gregg formatter
    eflambe:apply({arithmetic, multiply, [2, 3]}, Options),

    spawn_link(fun() ->
                  timer:sleep(100),
                  12 = arithmetic:multiply(4, 3)
               end),

    {ok, _} = eflambe:capture({arithmetic, multiply, 2}, 1, Options),

    % Both write separate trace files
    {ok, UpdatedFiles} = file:list_dir("."),
    NewNumFiles = length(UpdatedFiles),
    NewNumFiles = NumFiles + 2,

    % Assert new files have correct file extension
    NewFiles = UpdatedFiles -- Files,
    lists:foreach(fun(Filename) ->
                          ".bggg" = filename:extension(Filename)
                  end, NewFiles),

    ok = application:stop(eflambe).

multiple_captures(_Config) ->
    Options = [{output_format, brendan_gregg}],
    % Count files in dir
    {ok, Files} = file:list_dir("."),
    NumFiles = length(Files),

    % Capturing multiple calls should result in multiple output files
    spawn_link(fun() ->
                  timer:sleep(100),
                  12 = arithmetic:multiply(4, 3),
                  20 = arithmetic:multiply(5, 4)
               end),

    {ok, [_Filename1, _Filename2]} = eflambe:capture({arithmetic, multiply, 2}, 2, Options),

    % Both write separate trace files
    {ok, UpdatedFiles} = file:list_dir("."),
    NewNumFiles = length(UpdatedFiles),
    NewNumFiles = NumFiles + 2,

    % Assert new files have correct file extension
    NewFiles = UpdatedFiles -- Files,
    lists:foreach(fun(Filename) ->
                          ".bggg" = filename:extension(Filename)
                  end, NewFiles),

    ok = application:stop(eflambe).

capture_same_module(_Config) ->
    Options = [{output_format, brendan_gregg}],
    MFA = {arithmetic, multiply, 2},

    % Count files in dir
    {ok, Files} = file:list_dir("."),
    NumFiles = length(Files),

    spawn_link(fun() ->
                  timer:sleep(100),
                  12 = arithmetic:multiply(4, 3)
               end),

    % This second call should fail and return immediately
    spawn_link(fun() ->
                       timer:sleep(100),
                       {error, already_mecked} = eflambe:capture(MFA, 1, Options)
               end),

    % First call should succeed
    {ok, [_Filename]} = eflambe:capture(MFA, 1, Options),


    % First call should succeed and write trace file
    {ok, UpdatedFiles} = file:list_dir("."),
    NewNumFiles = length(UpdatedFiles),
    NewNumFiles = NumFiles + 1,

    % Assert new files have correct file extension
    NewFiles = UpdatedFiles -- Files,
    lists:foreach(fun(Filename) ->
                          ".bggg" = filename:extension(Filename)
                  end, NewFiles),


    spawn_link(fun() ->
                       timer:sleep(100),
                       12 = arithmetic:multiply(4, 3)
               end),

    % Should behave the same when run a second time
    {ok, [_Filename2]} = eflambe:capture({arithmetic, multiply, 2}, 1, Options),

    ok = application:stop(eflambe).
