%%%-------------------------------------------------------------------
%%% @doc
%%% This module defines a tracer process implemented as a gen_server. This
%%% gen_server is only intended to be used by the eflambe_server to receive
%%% trace messages and write them to the appropriate formatter module.
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
         handle_continue/2,
         handle_info/2]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_OPTIONS, [{output_format, brendan_gregg}]).

-record(state, {
          impl :: atom(),
          impl_state :: any(),
          options :: eflambe:options(),
          filename :: file:filename_all(),
          return :: atom()
         }).

-opaque state() :: #state{}.
-type from() :: {pid(), Tag :: term()}.
-type tracer_options() :: [eflambe:option() | {pid, pid()}].

-export_type([state/0, from/0, tracer_options/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the tracer
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(tracer_options()) -> {ok, pid()} | ignore | {error, Error :: any()}.

start_link(Options) ->
    gen_server:start_link(?MODULE, [Options], []).

%%--------------------------------------------------------------------
%% @doc
%% Stop the tracer and finalize the trace data
%%
%% @end
%%--------------------------------------------------------------------
-spec finish(pid()) -> {ok, Results :: any()}.

finish(Pid) ->
    gen_server:call(Pid, finish, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: list(tracer_options())) -> {ok, state()}.

init([Options]) ->
    % Generate complete list of options by falling back to default list
    FinalOptions = merge(Options, ?DEFAULT_OPTIONS),

    % Select the right implementation module. Provide shorter names for included
    % modules
    Impl = case proplists:get_value(output_format, FinalOptions) of
               brendan_gregg -> eflambe_brendan_gregg;
               svg -> eflambe_svg;
               perf -> eflambe_perf;
               plain -> eflambe_plain;
               Other -> Other
           end,

    % Generate output filename
    {ok, Ext} = erlang:apply(Impl, extension, []),
    Filename = generate_filename(Ext),
    FullFilename = filename:join([output_directory(FinalOptions), Filename]),

    Return = proplists:get_value(return, FinalOptions),

    % Initialize implementation state
    {ok, State} = erlang:apply(Impl, init, [FinalOptions]),
    InitialState = #state{
                      impl = Impl,
                      impl_state = State,
                      options = FinalOptions,
                      filename = FullFilename,
                      return = Return
                     },
    {ok, InitialState}.

-spec handle_call(Request :: any(), from(), state()) ->
                                  {reply, Reply :: any(), state()} |
                                  {reply, Reply :: any(), state(), {continue, finish}}.

handle_call(finish, _From, #state{impl = Impl, impl_state = ImplState, options = Options,
                                 filename = Filename, return = Return} = State) ->

    Results = case Return of
                  flamegraph ->
                      % Format trace data for return
                      {ok, FlameGraph} = erlang:apply(Impl, finalize, [return, ImplState]),
                      FlameGraph;

                  _ ->
                      % Format the trace data and write to file
                      {ok, FileData} = erlang:apply(Impl, finalize, [return, ImplState]),

                      % Write the data to file
                      {ok, File} = file:open(Filename, [write, exclusive]),
                      ok = file:write(File, FileData),
                      ok = file:close(File),

                      % Open flamegraph viewer if specified
                      maybe_open_in_program(Options, Filename),
                      io:format("Output filename: ~s~n", [Filename]),
                      Filename
              end,

    % The only reason we don't stop here is because this is a call and the
    % linked call would crash as well. This feels kind of wrong so I may revisit
    % this
    {reply, {ok, Results}, State, {continue, finish}}.

-spec handle_cast(any(), state()) -> {noreply, state()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_continue(finish, State) ->
    {stop, normal, State}.

-spec handle_info(Info :: any(), state()) -> {noreply, state()} |
                                  {noreply, state(), timeout()} |
                                  {stop, Reason :: any(), state()}.

handle_info(TraceMessage, #state{impl = Impl, impl_state = ImplState} = State)
  when element(1, TraceMessage) == trace; element(1, TraceMessage) == trace_ts ->
    {ok, UpdatedImplState} = erlang:apply(Impl, handle_trace_event, [TraceMessage, ImplState]),
    {noreply, State#state{impl_state = UpdatedImplState}};

handle_info(Info, State) ->
    logger:error("Received unexpected info message: ~w", [Info]),
    {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% https://stackoverflow.com/questions/21873644/combine-merge-two-erlang-lists
merge(In1, In2) ->
    Combined = In1 ++ In2,
    Fun = fun(Key) ->
                  [FinalValue|_] = proplists:get_all_values(Key, Combined),
                  {Key, FinalValue}
          end,
    lists:map(Fun, proplists:get_keys(Combined)).

timestamp_integer() ->
    {Mega, Secs, Micro} = erlang:timestamp(),
    Mega*1000*1000*1000*1000 + Secs*1000*1000 + Micro.

generate_filename(Ext) ->
    Name = io_lib:format("~B-~s.~s", [timestamp_integer(), <<"eflambe-output">>, Ext]),
    list_to_binary(Name).

output_directory(Options) ->
    proplists:get_value(output_directory, Options, "./").

maybe_open_in_program(Options, Filename) ->
    case proplists:get_value(open, Options) of
        undefined -> ok;
        Program when Program =:= speedscope; Program =:= hotspot ->
            _ = os:cmd(io_lib:format("~s ~s~n", [Program, Filename]));
        _InvalidProgram -> ok
    end.
