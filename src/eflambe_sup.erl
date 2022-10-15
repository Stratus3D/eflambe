%%%-------------------------------------------------------------------
%% @doc eflambe top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(eflambe_sup).

-behaviour(supervisor).

% API
-export([start_link/0, start_trace/2]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()} | ignore | {error, Error :: any()}.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_trace(MFA :: mfa(), Options :: list()) -> {ok, pid()} | {error, already_mecked}.

start_trace(MFA, Options) ->
    % This feels hacky but we MAY need the gen_server to send us all the results
    % right before it shuts down. Is there a gen_server or proc_lib function
    % that provides a better abstraction for one-off messages FROM a gen_server
    % like this?
    %
    % It's as the initial start_child start_link call should be treated as a
    % regular gen_server call and we need to wait on a response.
    Ref = make_ref(),
    case supervisor:start_child(?SERVER, [MFA, [{callback, {self(), Ref}}|Options]]) of
        {ok, _Child} ->
            receive
                {Ref, Msg} ->
                    io:format("Msg: ~p~n", [Msg]),
                    {ok, Msg}
            end;
        {error, _} = Error -> Error
    end.


%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 1,
                 period => 5},

    ChildSpecs = [#{id => eflambe_server,
                    start => {eflambe_server, start_link, []},
                    restart => temporary,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [eflambe_server]}],

    {ok, {SupFlags, ChildSpecs}}.
