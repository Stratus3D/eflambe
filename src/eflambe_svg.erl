%%%-------------------------------------------------------------------
%%% @copyright 2021 Trevor Brown (Apache-2.0 License)
%%% @doc
%%% Generates SVG graphics from trace messages.
%%% @end
%%%-------------------------------------------------------------------
-module(eflambe_svg).

-behaviour(eflambe_output_formatter).

-export([extension/0, init/1, handle_trace_event/2, finalize/2]).

-record(state, {
          options :: eflambe:options(),
          stack = [] :: list(),
          accumulator = [] :: list(),
          useconds = 0 :: integer()
         }).

-record(call, {
          mfa :: mfa() | atom(),
          depth = 0 :: integer(),
          start = 0 :: integer(),
          width = 1 :: integer()
         }).

% Constants for text display
-define(AVERAGE_CHAR_WIDTH, 0.59).
-define(FONT_SIZE, 12).
-define(CHAR_WIDTH, ?FONT_SIZE * ?AVERAGE_CHAR_WIDTH).

% Constants for flame graph canvas
-define(WIDTH, 800).
-define(YPAD1, ?FONT_SIZE * 4).
-define(YPAD2, (?FONT_SIZE * 2) + 10).
-define(XPAD, 10).

% Constants for flame graph block elements
-define(BLOCK_HEIGHT, 16).
-define(FRAMEPAD, 1).


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

handle_trace_event({trace_ts, _Pid, return_to, _MFA, _TS}, State) ->
    {ok, State};

% Ignore garbage collection minor start
handle_trace_event({trace_ts, _Pid, gc_minor_start, _Stats, _TS}, #state{} = State) ->
    {ok, State};

% Ignore garbage collection minor end
handle_trace_event({trace_ts, _Pid, gc_minor_end, _Stats, _TS}, #state{} = State) ->
    {ok, State};

% Ignore garbage collection major start
handle_trace_event({trace_ts, _Pid, gc_major_start, _Stats, _TS}, #state{} = State) ->
    {ok, State};

% Ignore garbage collection major end
handle_trace_event({trace_ts, _Pid, gc_major_end, _Stats, _TS}, #state{} = State) ->
    {ok, State};

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

    % We'll scale individual calls by the total width of the flame graph
    TotalSamples = length(Acc),
    WidthPerUnit = (?WIDTH - (2 * ?XPAD)) / TotalSamples,

    % Heigh is deteremined by the deepest call stack
    Depths = lists:map(fun(#call{depth=Depth}) -> Depth end, CallBlocks),
    MaxCallDepth = lists:max(Depths),
    Height = (MaxCallDepth * ?BLOCK_HEIGHT) + ?YPAD1 + ?YPAD2,

    % Post-process calls
    SvgBlocks = callblocks_to_svg(CallBlocks, TotalSamples, WidthPerUnit, Height),

    PrivDir = code:priv_dir(eflambe),
    {ok, HeaderBinary} = file:read_file(filename:join([PrivDir, "static/svg_header.svg"])),

    % Header elements
    HeaderElements = setup_svg_doc(?WIDTH, Height),

    {ok, [HeaderBinary, HeaderElements, SvgBlocks, <<"</svg>">>]}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% Helper function
svg(Element) -> xmerl:export_simple_element(Element, xmerl_xml).

svg_doc_elem(Width, Height) ->
    % We can't use xmerl here because I don't want to close the SVG doc in this
    % function, due to poor choices made earlier.
    ComputedViewbox = io_lib:format("0 0 ~B ~B", [Width, Height]),
    [
     <<"<svg version=\"1.1\" width=\"100%\" onload=\"init(evt)\" viewBox=\"">>,
     ComputedViewbox,
     <<"\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">">>
    ].

setup_svg_doc(Width, Height) ->
    Black = "rgb(0,0,0)",

    % TODO: Should convert this to xmerl
    Gradient = <<"<defs>
        <linearGradient id=\"background\" y1=\"0\" y2=\"1\" x1=\"0\" x2=\"0\" >
            <stop stop-color=\"#eeeeee\" offset=\"5%\" />
            <stop stop-color=\"#eeeeb0\" offset=\"95%\" />
        </linearGradient>
    </defs>">>,

    [
     svg_doc_elem(Width, Height),
     Gradient,
     % Background gradient
     svg({rect, [{x, 0},
                 {y, 0},
                 {width, Width},
                 {height, Height},
                 {fill, "url(#background)"}], []}),
     % Title
     svg({text, [{'text-anchor', "middle"},
                 {x, floor(Width/2)},
                 {y, ?FONT_SIZE * 2},
                 {'font-size', ?FONT_SIZE + 5},
                 {fill, Black}], ["Flame Graph"]}),
     % Details hover over
     svg({text, [{x, ?XPAD},
                 {y, floor(Height - ?YPAD2 / 2)},
                 {'font-size', ?FONT_SIZE},
                 {fill, Black},
                 {id, "details"}], [" "]}),
     % Unzoom button
     svg({text, [{x, ?XPAD},
                 {y, ?FONT_SIZE * 2},
                 {'font-size', ?FONT_SIZE},
                 {fill, Black},
                 {id, "unzoom"},
                 {onclick, "unzoom()"},
                 {style, "opacity:0.0;cursor:pointer"}], ["Reset Zoom"]})
    ].

callstacks_to_blocks(#state{accumulator = Acc}) ->
    callstacks_to_blocks(Acc, 0, [], []).

callstacks_to_blocks([], _Position, Incomplete, Complete) ->
    Incomplete ++ Complete;
callstacks_to_blocks([Callstack|Callstacks], Pos, Incomplete, Complete) ->
    {NewIncomplete, NewComplete} = apply_new_callstack(Callstack, 0, Pos, Incomplete, Complete),
    callstacks_to_blocks(Callstacks, Pos+1, NewIncomplete, NewComplete).

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
                    {NewComplete, RemIncomplete} = pop_any_deeper(Depth, Incomplete),
                    NewIncomplete = [NewCall|RemIncomplete],
                    FinalComplete = NewComplete ++ Complete,
                    apply_new_callstack(Callstack, Depth+1, Pos, NewIncomplete, FinalComplete)
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

callblocks_to_svg([], _Samples, _WidthPerUnit, _Height) -> [];
callblocks_to_svg([Call|Blocks], Samples, WidthPerUnit, Height) ->
    Svg = callblock_to_svg(Call, Samples, WidthPerUnit, Height),
    [Svg|callblocks_to_svg(Blocks, Samples, WidthPerUnit, Height)].

callblock_to_svg(#call{mfa=MFA, depth=Depth, start=Start, width=Width},
                 Samples, WidthPerUnit, Height) ->
    Name = generate_name(MFA, Depth),
    Percentage = (Width / Samples) * 100,
    MouseOverLabel = mouseoverlabel(Name, Width, Percentage),
    ColorFill = svg_color(Name),

    % Compute X and Y coordinates for call block SVG rectangle
    XStart = ?XPAD + (Start * WidthPerUnit),
    XEnd = ?XPAD + ((Start + Width) * WidthPerUnit),
    YTop = (Height - ?YPAD1) - ((Depth + 1) * ?BLOCK_HEIGHT) + ?FRAMEPAD,
    YBottom = (Height - ?YPAD1) - (Depth * ?BLOCK_HEIGHT),

    XWidth = XEnd - XStart,
    YHeight = YBottom - YTop,

    TrimmedText = trimmed_name(Name, XWidth),

    % Return the iolist representing the SVG group for the call block
    % TODO: Use xmerl here
    [
     <<"<g class=\"func_g\" onmouseover=\"s('">>,
     MouseOverLabel,
     <<"')\" onmouseout=\"c()\" onclick=\"zoom(this)\">">>,
    <<"<title>">>, MouseOverLabel,
    <<"</title><rect x=\"">>,
    format_number(XStart),
    <<"\" y=\"">>, format_number(YBottom), <<"\" width=\"">>,
    format_number(XWidth), <<"\" height=\"">>, format_number(YHeight), <<"\" fill=\"">>,
    ColorFill, <<"\" rx=\"2\" ry=\"2\" />">>,
    <<"<text text-anchor=\"\" x=\"">>,
    format_number(XStart), <<"\" y=\"">>,
    format_number(YBottom + ?FONT_SIZE),
    <<"\" font-size=\"">>,
    format_number(?FONT_SIZE), <<"\" fill=\"rgb(0,0,0)\"  >">>,
    TrimmedText,
    <<"</text></g>">>].

svg_color(<<"sleep">>) ->
    % Special blue color for sleeps
    "rgb(0,130,205)";
svg_color(Name) ->
    % This is probably the worst way to generate a different color for each
    % block but it is fast.
    Hash = crypto:hash(sha, Name),
    <<R/integer, G/integer, B/integer, _/binary>> = Hash,

    % Taken from https://github.com/proger/eflame/blob/master/flamegraph.pl#L303-L306
    Red = 205 + ceil((R / 255) * 50),
    Green = ceil((G / 255) * 220),
    Blue = ceil((B / 255) * 55),
    io_lib:format("rgb(~B,~B,~B)", [Red, Green, Blue]).

generate_name({Module, Function, Arity}, _Depth) ->
    iolist_to_binary(io_lib:format("~s:~s/~B", [Module, Function, Arity]));
generate_name(Other, _Depth) ->
    iolist_to_binary(io_lib:format("~s", [Other])).

trimmed_name(Name, Width) ->
    MaxChars = floor(Width / ?CHAR_WIDTH),
    NameChars = string:length(Name),
    case NameChars =< MaxChars of
        true -> Name;
        false -> string:slice(Name, 0, MaxChars)
    end.

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
