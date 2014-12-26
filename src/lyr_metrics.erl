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
-export([new_histogram/2]).
-export([incr/1]).
-export([get_gauges/0]).
-export([get_counters/0]).
-export([get_histograms/0]).
-export([vm/0]).
-export([i/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 1000).
-define(LOG(Level,Fmt), lager:Level(Fmt)).
-define(LOG(Level, Fmt, Args), lager:Level(Fmt, Args)).

-record(state, {gauges = [],
                counters = [],
                histograms = []}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new_counter(Name) ->
    gen_server:call(?SERVER, {new, counter, Name}).

new_gauge(Name, {_Fun, _Args} = Callback) ->
    gen_server:call(?SERVER, {new, gauge, Name, Callback}).

new_histogram(Name, {_Fun, _Args} = Callback) ->
    gen_server:call(?SERVER, {new, histogram, Name, Callback}).

get_gauges() ->
    gen_server:call(?SERVER, {get, gauges}).

get_counters() ->
    gen_server:call(?SERVER, {get, counters}).

get_histograms() ->
    gen_server:call(?SERVER, {get, histograms}).

-spec incr(list(atom())) -> ok | {error, not_found}.
incr(Counter) ->
    gen_server:cast(?SERVER, {incr, Counter}).

vm() ->
    VMStats = exometer:get_values([erlang]),
    [{pp(Name), Value} || {Name, [{value, Value}, {ms_since_reset, _}]} <- VMStats].

i() ->
    gen_server:call(?SERVER, info).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    case application:get_env(exometer, predefined) of
        {ok, Predefined} ->
            Gauges = [ {Name, metric_to_fun(Name)} || {Name, gauge, _Opt} <- Predefined ],
            ?LOG(info, "~p has configured ~p gauges", [?MODULE, length(Gauges)]),
            Histograms = [ {Name, metric_to_fun(Name)} || {Name, histogram, _Opt} <- Predefined ],
            ?LOG(info, "~p has configured ~p histograms", [?MODULE, length(Histograms)]),
            Counters = [ Name || {Name, counter, _Opt} <- Predefined ],
            ?LOG(info, "~p has configured ~p counters", [?MODULE, length(Counters)]),
            {ok, #state{gauges=Gauges, histograms=Histograms, counters=Counters}, ?TIMEOUT};
        undefined ->
            ?LOG(info, "~p has found no metrics", [?MODULE]),
            {ok, #state{}}
    end.

handle_call({new, counter, Name}, _From, #state{counters = Counters} = State) ->
    Result = exometer:new(Name, counter),
    ?LOG(info, "~p has installed a new counter: ~p", [?MODULE, Name]),
    {reply, Result, State#state{counters = [Name | Counters]}};
handle_call({new, Type, Name, Callback}, _From, #state{gauges = G, histograms = H} = State) 
  when Type =:= gauge; Type =:= histogram ->
    case is_callback(Callback) of
        true ->
            ok = exometer:new(Name, Type),
            NewState = 
                case Type of
                    gauge ->
                        State#state{gauges = [{Name, Callback} | G]};
                    histogram ->
                        State#state{histograms = [{Name, Callback} | H]}
                end,
            ?LOG(info, "~p has installed a new ~p: ~p", [?MODULE, Type, Name]),
            {reply, ok, NewState};
        false ->
            {reply, {error, badarg}, State}
    end;
handle_call({get, Types}, _From, State) 
  when Types =:= gauges; Types =:= histograms; Types =:= counters ->
    Result = [ begin 
                   Name = metric_name(Metric),
                   {ok, Info} = exometer:get_value(Name),
                   Value = proplists:get_value(value, Info, Info), 
                   {Name, Value} 
               end || Metric <- return(Types, State) ],
    {reply, Result, State};
handle_call(info, _From, #state{counters = C,
                                histograms = H,
                                gauges = G} = State) ->
    Result = 
        [{total, length(C ++ H ++ G)},
         {counters, C},
         {histograms, H},
         {gauges, G}],
    %% Add a field with the timer ref if available.
    {reply, Result, State}.
    

handle_cast({incr, Name}, State) ->
    exometer:update(Name, 1), %% add pattern match
    {noreply, State}.

handle_info(timeout, #state{gauges = [], histograms = []} = State) ->
    {noreply, State};
handle_info(timeout, #state{gauges = G, histograms = H} = State) ->
    true = update_metrics(G ++ H),
    {noreply, State, ?TIMEOUT}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

update_metrics(Metrics) ->
    Results = [ok = exometer:update(metric_name(Metric),
                                    sample(Metric)) || Metric <- Metrics],
    lists:all(fun(ok) -> true; (_) -> false end, Results).

sample(Metric) ->
    sample(Metric, metric_callback(Metric)).

sample(Metric, {Fun, Args}) ->
    Result = erlang:apply(Fun, Args),
    ?LOG(debug, "Sampled ~p with value: ~p", [Metric, Result]),
    Result.

return(counters, #state{counters = Counters}) ->
    Counters;
return(gauges, #state{gauges = Gauges}) ->
    Gauges;
return(histograms, #state{histograms = Histograms}) ->
    Histograms.

metric_name({Name, _Callback}) ->
    Name;
metric_name(Name) ->
    Name.

metric_callback({_Name, Callback}) ->
    Callback.    

pp(Name) ->
    pp(Name, []).

pp([], Acc) ->
    erlang:list_to_binary(lists:reverse(Acc));
pp([Atom], Acc) when is_atom(Atom) ->
    pp([], [atom_to_list(Atom) | Acc]);
pp([Atom | Rest], Acc) when is_atom(Atom) ->
    pp(Rest, ["_", atom_to_list(Atom) | Acc]).

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


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

pp_test() ->
    ?assertEqual(<<"">>, pp([])),
    ?assertEqual(<<"random_name">>, pp([random_name])),
    ?assertEqual(<<"first_second">>, pp([first, second])),
    ?assertEqual(<<"a_b_c">>, pp([a,b,c])).

-endif.
