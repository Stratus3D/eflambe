-module(eflambe_svg_perl).

-behaviour(eflambe_output_formatter).

-export([extension/0, init/1, handle_trace_event/2, finalize/2]).

-record(state, {
          options :: eflambe:options(),
          stack = [] :: list(),
          accumulator = [] :: list(),
          complete_blocks = [] :: list(),
          incomplete_blocks = [] :: list(),
          useconds = 0 :: integer()
         }).

-record(call, {
          mfa :: mfa(),
          depth = 0 :: integer(),
          start = 0 :: integer(),
          width = 1 :: integer()
         }).

-define(WIDTH, 800).

% Constants for flame graph block elements
-define(BLOCK_HEIGHT, 16).

% Constants for text display
-define(AVERAGE_TEXT_WIDTH, 0.59).
-define(FONT_SIZE, 12).


extension() -> {ok, <<"svg">>}.

%%--------------------------------------------------------------------
%% This callback exists so the implementation can initialize its own internal
%% state.
%%
%%--------------------------------------------------------------------
init(Options) ->
    {ok, #state{options = Options}}.

%%--------------------------------------------------------------------
%% This callback exists so the implementation module can process each individual
%% trace event.
%%
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
handle_trace_event({trace_ts, _Pid, return_to, MFA, TS}, #state{stack=[_, MFA|Stack]} = State) ->
    generate_new_state(State, [MFA|Stack], TS);

% I don't think I need to worry about these traces
handle_trace_event({trace_ts, _Pid, return_to, _MFA, _TS}, State) ->
    State;

handle_trace_event(TraceEvent, State) ->
    logger:error("Received unexpected trace event: ~w", [TraceEvent]),
    {ok, State}.

%%--------------------------------------------------------------------
%% This callback exists so the implementation module can finalize processing of
%% the trace data.
%%
%%--------------------------------------------------------------------
finalize(_Type, #state{accumulator = Acc} = State) ->
    % Recurse over callstacks in Acc:
        % Recurse over calls in callstack, starting with the top call (e.g. bottom of the flamegraph):
            % If matching callstack in incomplete_blocks at the same level:
                % Increment call length for the call on incomplete_blocks by 1
            % Else if different call exists at same level:
                % Move call and all deeper calls on incomplete_blocks to complete_blocks
                % Push call to incomplete_blocks
            % Else no call exists at same level:
                % Push call to incomplete_blocks
    % Move calls on incomplete_blocks to complete_blocks pushing the deepest call first
    CallBlocks = callstacks_to_blocks(State),
    io:format("CallBlocks - Line ~p: ~p~n", [?LINE, CallBlocks]),

    % We'll scale individual calls by the total width of the flame graph
    TotalSamples = length(Acc),
    WidthPerUnit = ?WIDTH / TotalSamples,

    % Post-process calls
    SvgBlocks = callblocks_to_svg(CallBlocks, TotalSamples, WidthPerUnit),

    PrivDir = code:priv_dir(eflambe),
    {ok, HeaderBinary} = file:read_file(filename:join([PrivDir, "static/svg_header.svg"])),
    {ok, [HeaderBinary, SvgBlocks]}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

callstacks_to_blocks(#state{accumulator = Acc, incomplete_blocks = Incomplete, complete_blocks = Complete}) ->
    callstacks_to_blocks(Acc, 0, Incomplete, Complete).

callstacks_to_blocks([], _Position, Incomplete, Complete) ->
    Incomplete ++ Complete;
callstacks_to_blocks([Callstack|Callstacks], Position, Incomplete, Complete) ->
    {NewIncomplete, NewComplete} = apply_new_callstack(Callstack, 0, Position, Incomplete, Complete),
    callstacks_to_blocks(Callstacks, Position+1, NewIncomplete, NewComplete).

apply_new_callstack([], _Depth, _Position, Incomplete, Complete) ->
  {Incomplete, Complete};
apply_new_callstack([MFA|Callstack], Depth, Pos, Incomplete, Complete) ->
    case find_same_call({MFA, Depth}, Incomplete) of
        undefined ->
            NewCall = #call{mfa=MFA, depth=Depth, start=Pos},
            case call_at_depth(Depth, Incomplete) of
                undefined ->
                    % Push new call
                    apply_new_callstack(Callstack, Depth+1, Pos, [NewCall|Incomplete], Complete);
                _Call ->
                    % Move call, and any deeper if they exist, to complete list and push new call
                    {NewComplete, RemainingIncomplete} = pop_any_deeper(Depth, Incomplete),
                    apply_new_callstack(Callstack, Depth+1, Pos, [NewCall|RemainingIncomplete], NewComplete ++ Complete)
            end;
        #call{width=Width} = Call ->
            % Update call
            UpdatedIncomplete = update_call(Call#call{width=Width+1}, Incomplete),
            apply_new_callstack(Callstack, Depth+1, Pos, UpdatedIncomplete, Complete)
    end.

pop_any_deeper(Depth, Incomplete) ->
    Predicate = fun
                    (#call{depth=CallDepth}) when CallDepth >= Depth -> true;
                    (_) -> false
                end,

    lists:partition(Predicate, Incomplete).


find_same_call({MFA, Depth}, IncompleteCalls) ->
    Predicate = fun(#call{depth=CallDepth, mfa=CallMFA})
                      when CallDepth == Depth, CallMFA == MFA -> true;
                   (_) -> false
                end,
    case lists:filter(Predicate, IncompleteCalls) of
        [] -> undefined;
        [Call] -> Call
    end.

call_at_depth(Depth, IncompleteCalls) ->
    Predicate = fun(#call{depth=CallDepth}) when CallDepth == Depth -> true;
                   (_) -> false
                end,
    case lists:filter(Predicate, IncompleteCalls) of
        [] -> undefined;
        [Call] -> Call
    end.

update_call(#call{mfa=MFA, depth=Depth} = Call, IncompleteCalls) ->
    UpdateFun = fun
                    (#call{mfa=CallMFA, depth=CallDepth})
                      when CallMFA == MFA, CallDepth == Depth -> Call;
                    (IncompleteCall) -> IncompleteCall
                end,
    lists:map(UpdateFun, IncompleteCalls).

callblocks_to_svg([], _TotalWidth, _WidthPerUnit) -> [];
callblocks_to_svg([Call|Blocks], TotalWidth, WidthPerUnit) ->
    [callblock_to_svg(Call, TotalWidth, WidthPerUnit)|callblocks_to_svg(Blocks, TotalWidth, WidthPerUnit)].

callblock_to_svg(#call{mfa=MFA, depth=Depth, start=Start, width=Width}, TotalWidth, WidthPerUnit) ->
    Name = generate_name(MFA, Depth),
    NumSamples = Width,
    Percentage = (Width / TotalWidth) * 100,
    TrimmedText = trimmed_name(Name, Width, TotalWidth),
    MouseOverLabel = mouseoverlabel(Name, NumSamples, Percentage),
    ColorFill = svg_color(),

    % Compute X and Y coordinates for call block SVG rectangle
    XStart = Start * WidthPerUnit,
    XEnd = (Start + Width) * WidthPerUnit,
    YBottom = Depth * ?BLOCK_HEIGHT,
    YTop = (Depth + 1) * ?BLOCK_HEIGHT,

    XWidth = XEnd - XStart,
    YHeight = YTop - YBottom,

    % Return the iolist representing the SVG group for the call block
    [
     <<"<g class=\"func_g\" onmouseover=\"s('">>,
     MouseOverLabel,
     <<"')\" onmouseout=\"c()\" onclick=\"zoom(this)\">">>,
    <<"<title>">>, MouseOverLabel,
    <<"</title><rect x=\"">>,
    format_number(XStart),
    <<"\" y=\"">>, format_number(YBottom), <<"\" width=\"">>, format_number(XWidth), <<"\" height=\"">>, format_number(YHeight), <<"\" fill=\"">>,
    ColorFill, <<"\" rx=\"2\" ry=\"2\" />">>,
    <<"<text text-anchor=\"\" x=\"695.19\" y=\"59.5\" font-size=\"12\" fill=\"rgb(0,0,0)\"  >">>,
    TrimmedText,
    <<"</text></g>">>].

svg_color() ->
    "rgb(205,0,7)".

generate_name({Module, Function, Arity}, _Depth) ->
    iolist_to_binary(io_lib:format("~s:~s/~B", [Module, Function, Arity]));
generate_name(Other, _Depth) ->
    iolist_to_binary(io_lib:format("~s", [Other])).

trimmed_name(Name, _Width, _TotalWidth) ->
    % TODO: Not sure how to compute this
    <<A/utf8, B/utf8, C/utf8, _/binary>> = Name,
    <<A, B, C>>.

mouseoverlabel(Name, NumSamples, Percentage) ->
    io_lib:format("~s (~B samples, ~.2f%)", [Name, NumSamples, Percentage]).

generate_new_state(#state{useconds = 0} = OldState, NewStack, TS) ->
    {ok, OldState#state{useconds=us(TS), stack = NewStack}};
generate_new_state(#state{accumulator=Acc} = OldState, NewStack, TS) ->
    %Diff = us(TS) - Micro,
    % Copy paste
    StackRev = lists:reverse(NewStack),
    NewAcc = lists:append([StackRev], Acc),
    {ok, OldState#state{useconds=us(TS), accumulator=NewAcc, stack=NewStack}}.

us({Mega, Secs, Micro}) ->
    Mega*1000*1000*1000*1000 + Secs*1000*1000 + Micro.

format_number(Float) when is_float(Float) -> io_lib:format("~.2f", [Float]);
format_number(Int) when is_integer(Int) -> io_lib:format("~B", [Int]).
