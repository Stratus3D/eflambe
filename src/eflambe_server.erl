%%%-------------------------------------------------------------------
%%% @doc
%%% E flambe server stores state for all traces that have been started. When
%%% No traces remain the server will shut down automatically.
%%% @end
%%%-------------------------------------------------------------------
-module(eflambe_server).

-behaviour(gen_server).

%% API
-export([start_link/0, start_trace/3, stop_trace/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).


-record(state, {traces = [] :: [trace()]}).

-record(trace, {
          id :: any(),
          max_calls :: integer(),
          calls :: integer(),
          running :: boolean(),
          tracer :: pid(),
          options = [] :: list()
         }).

-type state() :: #state{}.
-type trace() :: #trace{}.
-type from() :: {pid(), Tag :: term()}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, Error :: any()}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_trace(Id, MaxCalls, Options) ->
    gen_server:call(?SERVER, {start_trace, Id, MaxCalls, Options}).

stop_trace(Id) ->
    gen_server:call(?SERVER, {stop_trace, Id}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: list()) -> {ok, state()}.

init([]) ->
    {ok, #state{}}.

-spec handle_call(Request :: any(), from(), state()) ->
                                  {reply, Reply :: any(), state()} |
                                  {reply, Reply :: any(), state(), timeout()}.

handle_call({start_trace, Id, CallLimit, Options}, {FromPid, _}, State) ->
    TracerOptions = [{pid, FromPid}|Options],
    case get_trace_by_id(State, Id) of
        undefined ->
            % Create new trace, spawn a tracer for the trace
            {ok, TracerPid} = eflambe_tracer:start_link(TracerOptions),
            UpdatedTrace = #trace{
                              id = Id,
                              max_calls = CallLimit,
                              calls = 1,
                              options = Options,
                              running = true,
                              tracer = TracerPid
                             },
            {reply, {ok, Id, true, TracerPid}, put_trace(State, UpdatedTrace)};
        #trace{max_calls = MaxCalls, calls = Calls, tracer = TracerPid, running = false}
          when Calls =:= MaxCalls ->
            {reply, {ok, Id, false, TracerPid}, State};
        #trace{max_calls = MaxCalls, calls = Calls, tracer = TracerPid, running = true}
          when Calls =:= MaxCalls ->
            {reply, {ok, Id, false, TracerPid}, State};
        #trace{calls = Calls, running = false} = Trace ->
            % Increment existing trace
            NewCalls = Calls + 1,
            % Create new trace, spawn a tracer for the trace
            {ok, TracerPid} = eflambe_tracer:start_link(TracerOptions),

            % Update number of calls
            UpdatedTrace = Trace#trace{calls = NewCalls, running = true, tracer = TracerPid},
            NewState = update_trace(State, Id, UpdatedTrace),
            {reply, {ok, Id, true, TracerPid}, NewState};
        #trace{options = Options, running = true, tracer = TracerPid} ->
            {reply, {ok, Id, false, TracerPid}, State}
    end;

handle_call({stop_trace, Id}, _From, State) ->
    case get_trace_by_id(State, Id) of
        undefined ->
            % No trace found
            {reply, {error, unknown_trace}, State};

        #trace{max_calls = MaxCalls, calls = Calls, options = Options, running = true,
               tracer = TracerPid} = Trace when Calls =:= MaxCalls ->
            ok = maybe_unload_meck(Options),
            eflambe_tracer:finish(TracerPid),
            NewState = update_trace(State, Id, Trace#trace{running = false}),
            {reply, {ok, Id, Calls, true, Options}, NewState};

        #trace{max_calls = MaxCalls, calls = Calls, options = Options, running = false
              } when Calls =:= MaxCalls ->
            ok = maybe_unload_meck(Options),
            {reply, {ok, Id, Calls, false, Options}, State};

        #trace{calls = Calls, options = Options, running = true, tracer = TracerPid} = Trace ->
            eflambe_tracer:finish(TracerPid),
            NewState = update_trace(State, Id, Trace#trace{running = false}),
            {reply, {ok, Id, Calls, true, Options}, NewState};

        #trace{calls = Calls, options = Options, running = false} ->
            % Trace already stopped
            {reply, {ok, Id, Calls, false, Options}, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(any(), state()) -> {noreply, state()} |
                                 {noreply, state(), timeout()} |
                                 {stop, Reason :: any(), state()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(Info :: any(), state()) -> {noreply, state()} |
                                  {noreply, state(), timeout()} |
                                  {stop, Reason :: any(), state()}.

handle_info(Info, State) ->
    logger:error("Received unexpected info message: ~w", [Info]),
    {noreply, State}.

-spec terminate(Reason :: any(), state()) -> any().

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_trace_by_id(#state{traces = Traces}, Id) ->
    case lists:filter(lookup_fun(Id), Traces) of
        [] -> undefined;
        [Trace] -> Trace
    end.

put_trace(#state{traces = ExistingTraces} = State, NewTrace) ->
    State#state{traces = [NewTrace|ExistingTraces]}.

update_trace(#state{traces = ExistingTraces} = State, Id, UpdatedTrace) ->
    {[_Trace], Rest} = lists:partition(lookup_fun(Id), ExistingTraces),
    State#state{traces = [UpdatedTrace|Rest]}.

lookup_fun(Id) ->
    fun
        (#trace{id = TraceId}) when TraceId =:= Id -> true;
        (_) -> false
    end.

maybe_unload_meck(Options) ->
    case proplists:get_value(meck, Options) of
        undefined -> ok;
        ModuleName ->
            % Unload if module has been mecked
            ok = meck:unload(ModuleName)
    end.
