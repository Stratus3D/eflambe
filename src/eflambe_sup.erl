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
    supervisor:start_child(?SERVER, [MFA, Options]).

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
