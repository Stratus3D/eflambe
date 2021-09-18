%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(eflambe_tracer).

-behaviour(gen_server).

%% API
-export([
         start_link/1,
         finish/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_OPTIONS, [{output_format, brendan_greggs}]).

-record(state, {options :: eflambe:options()}).

-type state() :: #state{}.
-type from() :: {pid(), Tag :: term()}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the tracer
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(eflambe:options()) -> {ok, pid()} | ignore | {error, Error :: any()}.

start_link(Options) ->
    gen_server:start_link(?MODULE, [Options], []).

finish(Pid) ->
    gen_server:call(Pid, finish).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: list()) -> {ok, state()}.

init(Options) ->
    % Generate complete list of options by falling back to default list
    FinalOptions = merge(Options, ?DEFAULT_OPTIONS),
    {ok, #state{options = FinalOptions}}.

-spec handle_call(Request :: any(), from(), state()) ->
                                  {stop, Reason :: any(), Reply :: any(), state()} |
                                  {stop, Reason :: any(), state()}.

handle_call(finish, _From, #state{options = Options} = State) ->
    % Format the trace data and write to file
    % TODO: Implement this

    {stop, normal, State};

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

handle_info(TraceMessage, State) when element(1, TraceMessage) == trace ->
    NewState = handle_trace_message(TraceMessage, State),
    % TODO: Remove this when handle_trace_message is sending the data to the output modules
    io:format("Received Trace Message: ~p~n", [TraceMessage]),
    {noreply, NewState};

handle_info(Info, State) ->
    logger:error("Received unexpected info message: ~w", [Info]),
    {noreply, State}.

-spec terminate(Reason :: any(), state()) -> any().

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% TODO: Use some of these trace messages
handle_trace_message({trace, PidPort, send, Msg, To}, State) -> State;
handle_trace_message({trace, PidPort, send_to_non_existing_process, Msg, To}, State) -> State;
handle_trace_message({trace, PidPort, 'receive', Msg}, State) -> State;
handle_trace_message({trace, Pid, call, {M, F, Args}}, State) -> State;
handle_trace_message({trace, Pid, return_to, {M, F, Arity}}, State) -> State;
handle_trace_message({trace, Pid, return_from, {M, F, Arity}, ReturnValue}, State) -> State;
handle_trace_message({trace, Pid, exception_from, {M, F, Arity}, {Class, Value}}, State) -> State;
handle_trace_message({trace, Pid, spawn, Pid2, {M, F, Args}}, State) -> State;
handle_trace_message({trace, Pid, spawned, Pid2, {M, F, Args}}, State) -> State;
handle_trace_message({trace, Pid, exit, Reason}, State) -> State;
handle_trace_message({trace, PidPort, register, RegName}, State) -> State;
handle_trace_message({trace, PidPort, unregister, RegName}, State) -> State;
handle_trace_message({trace, Pid, link, Pid2}, State) -> State;
handle_trace_message({trace, Pid, unlink, Pid2}, State) -> State;
handle_trace_message({trace, PidPort, getting_linked, Pid2}, State) -> State;
handle_trace_message({trace, PidPort, getting_unlinked, Pid2}, State) -> State;
handle_trace_message({trace, Port, open, Pid, Driver}, State) -> State;
handle_trace_message({trace, Port, closed, Reason}, State) -> State;
handle_trace_message({trace, Pid, in, MFA0}, State) -> State;
handle_trace_message({trace, Pid, in_exiting, MFA0}, State) -> State;
handle_trace_message({trace, Pid, out, MFA0}, State) -> State;
handle_trace_message({trace, Pid, out_exiting, MFA0}, State) -> State;
handle_trace_message({trace, Pid, out_exited, MFA0}, State) -> State;
handle_trace_message({trace, Port, in, Command0}, State) -> State;
handle_trace_message({trace, Port, out, Command0}, State) -> State;
handle_trace_message({trace, Pid, gc_minor_start, Info}, State) -> State;
handle_trace_message({trace, Pid, gc_max_heap_size, Info}, State) -> State;
handle_trace_message({trace, Pid, gc_minor_end, Info}, State) -> State;
handle_trace_message({trace, Pid, gc_major_start, Info}, State) -> State;
handle_trace_message({trace, Pid, gc_major_end, Info}, State) -> State;
handle_trace_message(_TraceMessage, State) -> State.

% https://stackoverflow.com/questions/21873644/combine-merge-two-erlang-lists
merge(In1, In2) ->
    Combined = In1 ++ In2,
    Fun = fun(Key) ->
                  [FinalValue|_] = proplists:get_all_values(Key, Combined),
                  {Key, FinalValue}
          end,
    lists:map(Fun, proplists:get_keys(Combined)).
