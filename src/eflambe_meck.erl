%%%-------------------------------------------------------------------
%%% @doc
%%% Helper functions for mocking with meck in eflambe
%%% @end
%%%-------------------------------------------------------------------
-module(eflambe_meck).

%% API
-export([shim/4, unload/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Takes a module, function, arity, and a shim function with an arity of 1
%% and sets up a mock module with the function of the specified arity replaced
%% with the shim function. The shim function will receive a list of arguments
%% from the call to the original un-mocked function as a single argument.
%%
%% @end
%%--------------------------------------------------------------------
-spec shim(atom(), atom(), integer(), fun()) -> ok | {error, already_mecked}.

shim(Module, Function, Arity, ShimFunction)
  when is_atom(Module), is_atom(Function), is_integer(Arity), is_function(ShimFunction)  ->
    try meck:new(Module, [unstick, passthrough]) of
        ok ->
            MockFun = gen_mock_fun(Arity, ShimFunction),

            % Replace the original function with our new function that wraps the old
            % function in profiling code.
            meck:expect(Module, Function, MockFun)
    catch
        error:{already_started, _Pid} ->
            {error, already_mecked}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Unload meck mock
%%
%% @end
%%--------------------------------------------------------------------
-spec unload(atom()) -> ok.

unload(Module) when is_atom(Module) ->
    ok = meck:unload(Module).

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
