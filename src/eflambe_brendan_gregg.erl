%%%-------------------------------------------------------------------
%%% @doc
%%% Writes raw trace messages to file. This output format is for testing.
%%% Writes trace data to file formatted as Brendan Gregg's collapsed stack
%%% format.
%%%
%%% See https://github.com/brendangregg/FlameGraph/blob/master/stackcollapse.pl
%%%
%%% This file was heavily inspired by
%%% https://github.com/proger/eflame/blob/master/src/eflame.erl
%%% @end
%%%-------------------------------------------------------------------
-module(eflambe_brendan_gregg).

-behaviour(eflambe_output_formatter).

-export([extension/0, init/2, handle_trace_event/2, finalize/2]).

-record(state, {
          file :: any(),
          options :: eflambe:options(),
          stack = [] :: list(),
          accumulator = [] :: list(),
          microseconds = 0 :: integer()
         }).

extension() -> {ok, <<"bggg">>}.

%%--------------------------------------------------------------------
%% @doc
%% This callback exists so the implementation can initialize its own internal
%% state. This may be useful for opening files, etc...
%%
%% @end
%%--------------------------------------------------------------------
init(Filename, Options) ->
    {ok, File} = file:open(Filename, [write, exclusive]),
    {ok, #state{file = File, options = Options}}.

%%--------------------------------------------------------------------
%% @doc
%% This callback exists so the implementation module can process each individual
%% trace event.
%%
%% @end
%%--------------------------------------------------------------------

% Anytime a call event is received and we have an empty stack push both the caller
% and the call itself onto the stack
handle_trace_event({trace_ts, _Pid, call, MFA, {cp, CallerMFA}, TS}, #state{stack = []} = State) ->
    generate_new_state(State, [MFA, CallerMFA], TS);

% If there is no caller and the stack is empty just push the call itself
handle_trace_event({trace_ts, _Pid, call, MFA, {cp, undefined}, TS}, #state{stack = []} = State) ->
    generate_new_state(State, [MFA], TS);

% When the current call is the same call as the one at the top of the stack
% don't change anything except the timestamp
handle_trace_event({trace_ts, _Pid, call, MFA, {cp, undefined}, TS},
                   #state{stack = [MFA|_]} = State) ->
    generate_new_state(State, [MFA], TS);

% When the current call is different than the one at the top of the stack push
% the new call and new timestamp
handle_trace_event({trace_ts, _Pid, call, MFA, {cp, undefined}, TS},
                   #state{stack = Stack} = State) ->
    generate_new_state(State, [MFA|Stack], TS);

% If a function calls itself we shouldn't push a new call onto the stack.
% Otherwise we could end up with infinitely tall flamegraphs. We are
% effectively collapsing multiple recursive calls down into one here
handle_trace_event({trace_ts, _Pid, call, MFA, {cp, MFA}, TS},
                   #state{stack = [MFA|Stack]} = State) ->
    generate_new_state(State, [MFA|Stack], TS);

% Handle the case of a normal call with the calling function already on the stack
handle_trace_event({trace_ts, _Pid, call, MFA, {cp, CallingMFA}, TS},
                   #state{stack = [CallingMFA|Stack]} = State) ->
    generate_new_state(State, [MFA, CallingMFA|Stack], TS);

% Must have been a call from a function that is not at the top of the stack.
% Move up one level and look for a match.
handle_trace_event({trace_ts, _Pid, call, _MFA, {cp, _CallingMFA}, _} = Trace,
                   #state{stack = [_|StackRest]} = State) ->
    handle_trace_event(Trace, State#state{stack = StackRest});

% Process asleep
handle_trace_event({trace_ts, _Pid, in, _Command0, TS}, #state{stack = [sleep|Stack]} = State) ->
    generate_new_state(State, [sleep|Stack], TS);

% Process is scheduled in, only change timestamp
handle_trace_event({trace_ts, _Pid, in, _Command0, TS}, #state{stack = Stack} = State) ->
    generate_new_state(State, Stack, TS);

% Process starts to sleep
handle_trace_event({trace_ts, _Pid, out, _Command0, TS}, #state{stack = Stack} = State) ->
    generate_new_state(State, [sleep|Stack], TS);

% Function returned to a caller higher up on the stack
handle_trace_event({trace_ts, _Pid, return_to, MFA, TS}, #state{stack=[_Current, MFA|Stack]} = State) ->
    generate_new_state(State, [MFA|Stack], TS);

% I don't think I need to worry about these traces
handle_trace_event({trace_ts, _Pid, return_to, _MFA, _TS}, State) ->
    State;

%handle_trace_event({trace_ts, PidPort, send, Msg, To}, State) -> State;
%handle_trace_event({trace_ts, PidPort, send_to_non_existing_process, Msg, To}, State) -> State;
%handle_trace_event({trace_ts, PidPort, 'receive', Msg}, State) -> State;
%handle_trace_event({trace_ts, Pid, return_to, {M, F, Arity}}, State) -> State;
%handle_trace_event({trace_ts, Pid, return_from, {M, F, Arity}, ReturnValue}, State) -> State;
%handle_trace_event({trace_ts, Pid, exception_from, {M, F, Arity}, {Class, Value}}, State) -> State;
%handle_trace_event({trace_ts, Pid, spawn, Pid2, {M, F, Args}}, State) -> State;
%handle_trace_event({trace_ts, Pid, spawned, Pid2, {M, F, Args}}, State) -> State;
%handle_trace_event({trace_ts, Pid, exit, Reason}, State) -> State;
%handle_trace_event({trace_ts, PidPort, register, RegName}, State) -> State;
%handle_trace_event({trace_ts, PidPort, unregister, RegName}, State) -> State;
%handle_trace_event({trace_ts, Pid, link, Pid2}, State) -> State;
%handle_trace_event({trace_ts, Pid, unlink, Pid2}, State) -> State;
%handle_trace_event({trace_ts, PidPort, getting_linked, Pid2}, State) -> State;
%handle_trace_event({trace_ts, PidPort, getting_unlinked, Pid2}, State) -> State;
%handle_trace_event({trace_ts, Port, open, Pid, Driver}, State) -> State;
%handle_trace_event({trace_ts, Port, closed, Reason}, State) -> State;
%handle_trace_event({trace_ts, Pid, in, MFA0}, State) -> State;
%handle_trace_event({trace_ts, Pid, in_exiting, MFA0}, State) -> State;
%handle_trace_event({trace_ts, Pid, out, MFA0}, State) -> State;
%handle_trace_event({trace_ts, Pid, out_exiting, MFA0}, State) -> State;
%handle_trace_event({trace_ts, Pid, out_exited, MFA0}, State) -> State;
%handle_trace_event({trace_ts, Pid, gc_minor_start, Info}, State) -> State;
%handle_trace_event({trace_ts, Pid, gc_max_heap_size, Info}, State) -> State;
%handle_trace_event({trace_ts, Pid, gc_minor_end, Info}, State) -> State;
%handle_trace_event({trace_ts, Pid, gc_major_start, Info}, State) -> State;
%handle_trace_event({trace_ts, Pid, gc_major_end, Info}, State) -> State;

handle_trace_event(TraceEvent, State) ->
    logger:error("Received unexpected trace event: ~w", [TraceEvent]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% This callback exists so the implementation module can finalize processing of
%% the trace data. For example, any last minute formatting or flushing of data
%% in state to disk.
%%
%% @end
%%--------------------------------------------------------------------
finalize(_Options, #state{file = File} = State) ->
    % TODO: Using self() is wrong here. We should probably provide the real pid
    % TODO: Correct the formatting here - (uniq -c | awk '{print $2, " ", $1}')
    ok = file:write(File, dump_to_iolist(self(), State)),
    ok = file:close(File),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

generate_new_state(#state{microseconds = 0} = OldState, NewStack, Timestamp) ->
    {ok, OldState#state{microseconds=us(Timestamp), stack = NewStack}};
generate_new_state(#state{file = File, microseconds = Micro, accumulator=Acc} = OldState, NewStack, Timestamp) ->
    %Diff = us(Timestamp) - Micro,
    % Copy paste
    StackRev = lists:reverse(NewStack),
    Stacks = [StackRev || _ <- lists:seq(1, 2)],
    io:format("Stacks: ~p~n", [Stacks]),
    {ok, OldState#state{microseconds=us(Timestamp), accumulator=lists:append(Stacks, Acc), stack=NewStack}}.

us({Mega, Secs, Micro}) ->
    Mega*1000*1000*1000*1000 + Secs*1000*1000 + Micro.

stack_collapse(Stack) ->
    intercalate(";", [entry_to_iolist(S) || S <- Stack]).

entry_to_iolist({M, F, A}) ->
    [atom_to_binary(M, utf8), <<":">>, atom_to_binary(F, utf8), <<"/">>, integer_to_list(A)];
entry_to_iolist(A) when is_atom(A) ->
    [atom_to_binary(A, utf8)].

dump_to_iolist(Pid, #state{accumulator=Acc}) ->
    [[pid_to_list(Pid), <<";">>, stack_collapse(S), <<"\n">>] || S <- lists:reverse(Acc)].

intercalate(Sep, Xs) -> lists:concat(intersperse(Sep, Xs)).

intersperse(_, []) -> [];
intersperse(_, [X]) -> [X];
intersperse(Sep, [X | Xs]) -> [X, Sep | intersperse(Sep, Xs)].
