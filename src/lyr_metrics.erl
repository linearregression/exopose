%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, Layer Inc.
%%% @doc
%%% Server that samples Erlang VM stats through exometer.
%%% @end
%%%-------------------------------------------------------------------
-module(lyr_metrics).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/0]).
-export([new_counter/1]).
-export([new_gauge/2]).
-export([incr/1]).
-export([sample/1,
         sample/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 1000).
-define(DEBUG(Fmt), lager:debug(Fmt)).
-define(DEBUG(Fmt, Args), lager:debug(Fmt, Args)).

-record(state, {gauges = [],
                counters = []}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new_counter(Name) ->
    gen_server:call(?SERVER, {new, counter, Name}).

new_gauge(Name, {_Fun, _Args} = Callback) ->
    gen_server:call(?SERVER, {new, gauge, Name, Callback}).

-spec incr(list(atom())) -> ok | {error, not_found}.
incr(Counter) ->
    gen_server:cast(?SERVER, {incr, Counter}).

sample(Metric) ->
    sample(Metric, metric_to_fun(Metric)).

sample(_Metric, {Fun, Args}) ->
    erlang:apply(Fun, Args).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    case application:get_env(exometer, predefined) of
        {ok, Predefined} ->
            Gauges = [ Name || {Name, gauge, _Opt} <- Predefined ],
            ?DEBUG("exometer gauges available ~p", [Gauges]),
            {ok, #state{gauges=Gauges}, ?TIMEOUT};
        undefined ->
            ?DEBUG("exometer has no available metrics"),
            {ok, #state{}}
    end.

handle_call({new, counter, Name}, _From, #state{counters = Counters} = State) ->
    Result = exometer:new(Name, counter),
    ?DEBUG("New counter ~p", [Name]),
    {reply, Result, State#state{counters = [Name | Counters]}};
handle_call({new, gauge, Name, Callback}, _From, #state{gauges = Gauges} = State) ->
    case is_callback(Callback) of
        true ->
            ?DEBUG("New gauge ~p", [Name]),
            ok = exometer:new(Name, gauge),
            {reply, ok, State#state{gauges = [{Name, Callback} | Gauges]}};
        false ->
            {reply, {error, badarg}, State}
    end.

handle_cast({incr, Name}, State) ->
    ok = exometer:update(Name, 1),
    {noreply, State}.

handle_info(timeout, #state{gauges = []} = State) ->
    {noreply, State};
handle_info(timeout, #state{gauges = Gauges} = State) ->
    true = exometer_update(Gauges),
    {noreply, State, ?TIMEOUT}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

exometer_update(Metrics) ->
    Results = [ok = exometer:update(Metric,
                                    sample(Metric)) || Metric <- Metrics],
    lists:all(fun(ok) -> true; (_) -> false end, Results).

metric_to_fun([erlang, processes]) ->
    {fun erlang:memory/1, [processes]};
metric_to_fun([erlang, system]) ->
    {fun erlang:memory/1, [system]};
metric_to_fun([erlang, atom]) ->
    {fun erlang:memory/1, [atom]};
metric_to_fun([erlang, binary]) ->
    {fun erlang:memory/1, [binary]};
metric_to_fun([erlang, ets]) ->
    {fun erlang:memory/1, [ets]};
metric_to_fun([erlang, run_queue]) ->
    {fun erlang:statistics/1, [run_queue]}.

is_callback({Fun, Args}) ->
    if is_function(Fun) andalso is_list(Args) ->
            is_integer(erlang:apply(Fun, Args));
       true ->
            false
    end.
