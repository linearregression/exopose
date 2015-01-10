%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, Layer Inc.
%%% @doc
%%% Server for easy metric management through `exometer`.
%%% @end
%%%-------------------------------------------------------------------
-module(exopose).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

-include("exopose.hrl").

%% API
-export([start_link/0]).
-export([new_counter/1]).
-export([new_gauge/2]).
-export([new_histogram/2]).
-export([incr/1]).
-export([get_gauges/0]).
-export([get_counters/0]).
-export([get_histograms/0]).
-export([get_timeout/0]).
-export([set_timeout/1]).
-export([vm/0]).
-export([i/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 10000).
-define(LOG(Level,Fmt), lager:Level(Fmt)).
-define(LOG(Level,Fmt,Args), lager:Level(Fmt,Args)).

-record(state, {timeout,
                callbacks}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec new_counter(name()) -> ok.
new_counter(Name) ->
    gen_server:call(?SERVER, {new, counter, Name}).

-spec new_gauge(name(), callback()) -> ok.
new_gauge(Name, Callback) ->
    gen_server:call(?SERVER, {new, gauge, Name, Callback}).

-spec new_histogram(name(), callback()) -> ok.
new_histogram(Name, Callback) ->
    gen_server:call(?SERVER, {new, histogram, Name, Callback}).

-spec get_gauges() -> list(gauge()).
get_gauges() ->
    gen_server:call(?SERVER, {get_type, gauge}).

get_counters() ->
    gen_server:call(?SERVER, {get_type, counter}).

get_histograms() ->
    gen_server:call(?SERVER, {get_type, histogram}).

%% @doc Returns current used timeout value for the sampling server.
-spec get_timeout() -> pos_integer().
get_timeout() ->
    gen_server:call(?SERVER, {get, timeout}).

%% @doc Sets a new timeout value for the sampling server.
-spec set_timeout(pos_integer()) -> ok.
set_timeout(T) ->
    gen_server:call(?SERVER, {set, timeout, T}).

%% @doc Increments the value of a given counter
-spec incr(list(atom())) -> ok | {error, not_found}.
incr(Counter) ->
    gen_server:cast(?SERVER, {incr, Counter}).

%% @doc Returns a list of Erlang VM metrics and their current values.
-spec vm() -> list(tuple(binary(), integer())).
vm() ->
    VMStats = exometer:get_values([vm]),
    [{pp(Name), Value} || {Name, [{value, Value}, {ms_since_reset, _}]} <- VMStats].

%% @doc Returns various information about current metrics state.
-spec i() -> list(tuple(atom(), term())).
i() ->
    gen_server:call(?SERVER, info).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ?LOG(info, "~p has started monotoring ~p metrics", [?MODULE, length(exometrics())]),
    Callbacks =
        case application:get_env(callbacks) of
            undefined -> [];
            {ok, List} -> List
        end,
    {ok, #state{timeout = ?TIMEOUT, callbacks = Callbacks}, ?TIMEOUT}.

handle_call({new, counter, Name}, _From, #state{timeout = T} = State) ->
    Result = exometer:new(Name, counter),
    ?LOG(info, "~p has installed a new counter: ~p", [?MODULE, Name]),
    {reply, Result, State, T};
handle_call({new, Type, Name, Callback}, _From, State)
  when Type =:= gauge; Type =:= histogram ->
    #state{callbacks = C, timeout = T} = State,
    case is_callback(Callback) of
        true ->
            ok = exometer:new(Name, Type),
            ?LOG(info, "~p has installed a new ~p: ~p", [?MODULE, Type, Name]),
            {reply, ok, State#state{callbacks = [{Name, Callback} | C]} , T};
        false ->
            {reply, {error, badarg}, State, T}
    end;
handle_call({get_type, Type}, _From, #state{timeout = T} = State)
  when Type =:= gauge; Type =:= histogram; Type =:= counter ->
    Result = [ begin 
                   {ok, Info} = exometer:get_value(Metric),
                   [{name, pp(Metric)} | Info]
               end || Metric <- exometrics(Type) ],
    {reply, Result, State, T};
handle_call({get, timeout}, _From, #state{timeout = T} = State) ->
    {reply, T, State, T};
handle_call({set, timeout, T}, _From, State) ->
    {reply, ok, State#state{timeout = T}, T};
handle_call(info, _From, #state{callbacks = C, timeout = T} = State) ->
    EM = exometrics(),
    Result = 
        [{total, length(EM)},
         {exometer_reporters, exometer_report:list_reporters()},
         {counters, exometrics(counter, EM)},
         {histograms, exometrics(histogram, EM)},
         {gauges, exometrics(gauge, EM)},
         {callbacks, C}],
    %% Add a field with the timer ref if available.
    {reply, Result, State, T}.
    
handle_cast({incr, Name}, #state{timeout = T} = State) ->
    exometer:update(Name, 1), %% add pattern match
    {noreply, State, T}.

%% Everytime the server times out, metrics are sampled and
%% new values are updated through `exometer:update/2`.
%% Given three main metrics: counters, histograms and gauges,
%% only histograms and guages need to be sampled through their
%% stored callbacks.
handle_info(timeout, #state{callbacks = Callbacks, timeout = T} = State) ->
    ?LOG(info, "Sampling metrics..."),
    Metrics = exometrics(gauge) ++ exometrics(histogram),
    [ok = begin
              case sample(Metric, Callbacks) of
                  skip -> ok;
                  Data -> exometer:update(Metric, Data)
              end
          end || Metric <- Metrics],
    {noreply, State, T}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec sample(name(), list(callback())) -> integer() | skip.
sample(Metric, Callbacks) ->
    case proplists:get_value(Metric, Callbacks) of
        undefined ->
            ?LOG(info, "Callback for metric ~p has not been found", [Metric]),
            skip;
        Callback ->
            apply_callback(Callback)
    end.

-spec apply_callback(callback()) -> integer().
apply_callback({Fun, Args}) when is_function(Fun) ->
    erlang:apply(Fun, Args);
apply_callback({{Mod, Fun, _Arity}, Args}) ->
    erlang:apply(Mod, Fun, Args).

-spec exometrics() -> list(tuple(name(), atom())).
exometrics() ->
    {ok, ExoMetrics} = exometer_report:list_metrics(),
    [ {Name, proplists:get_value(type, exometer:info(Name))} || {Name, _DP, _Opt, enabled} <- ExoMetrics ].

-spec exometrics(atom()) -> list(name()).
exometrics(Type) ->
    exometrics(Type, exometrics()).

-spec exometrics(atom(), list(tuple(name(), atom()))) -> list(name()).
exometrics(Type, Metrics) ->
    lists:filtermap(fun({Name, T}) when T =:= Type -> {true, Name}; ({_,_}) -> false end, Metrics).

%% Pretty printing of exometer's metric names
-spec pp(name()) -> binary().
pp(Name) ->
    pp(Name, []).

pp([], Acc) ->
    erlang:list_to_binary(lists:reverse(Acc));
pp([Atom], Acc) when is_atom(Atom) ->
    pp([], [atom_to_list(Atom) | Acc]);
pp([Atom | Rest], Acc) when is_atom(Atom) ->
    pp(Rest, ["_", atom_to_list(Atom) | Acc]).

-spec is_callback(callback()) -> boolean().
is_callback({Fun, Args}) when is_function(Fun) ->
    if is_list(Args) ->
            is_integer(erlang:apply(Fun, Args));
       true ->
            false
    end;
is_callback({{M,F,_A}, Args}) ->
    if is_list(Args) ->
            is_integer(erlang:apply(M,F,Args));
       true ->
            false
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

pp_test() ->
    ?assertEqual(<<"">>, pp([])),
    ?assertEqual(<<"random_name">>, pp([random_name])),
    ?assertEqual(<<"first_second">>, pp([first, second])),
    ?assertEqual(<<"a_b_c">>, pp([a,b,c])).

is_callback_test() ->
    ?assertNot(is_callback({fun erlang:now/0, []})),
    ?assertNot(is_callback({fun lists:seq/2, [1,5]})),
    ?assert(is_callback({fun () -> 1 end, []})),
    ?assert(is_callback({fun (N) -> N+1 end, [0]})).

-endif.