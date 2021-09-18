%%%-------------------------------------------------------------------
%% @doc eflambe top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(eflambe_sup).

-behaviour(supervisor).

% API
-export([start_link/0, get_or_start_server/0]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()} | ignore | {error, Error :: any()}.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec get_or_start_server() -> {ok, pid()}.

get_or_start_server() ->
    case supervisor:restart_child(?SERVER, eflambe_server) of
        {error, running} ->
            {ok, whereis(eflambe_server)};
        {ok, Server} -> {ok, Server}
    end.

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    ChildSpecs = [#{id => eflambe_server,
                    start => {eflambe_server, start_link, []},
                    restart => temporary,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [eflambe_server]}],

    {ok, {SupFlags, ChildSpecs}}.
