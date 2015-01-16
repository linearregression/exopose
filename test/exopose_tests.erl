-module(exopose_tests).

-include_lib("eunit/include/eunit.hrl").

-include("exopose.hrl").

exopose_server_test_() ->
    {setup,
     fun() ->
             {ok, Started} = application:ensure_all_started(exopose),
             ?assertEqual(ok, exopose:set_timeout(100)),
             Started
     end,
     fun(Started) ->
             [ ok = application:stop(App) || App <- lists:reverse(Started), App =/= lager ]
     end,
     [
      {"Server starts with pre-configured Erlang VM metrics",
       fun() ->
               ?assert(length(exopose:vm()) > 0)
       end},
      {"Server samples data using defined Erlang VM callbacks",
       fun() ->
               VMMetrics = exopose:vm(),
               %% Excluding `erlang:statistics(run_queue)` since that can be 0.
               [ ?assert(Val > 0) || {Name, Val} <- VMMetrics, Name =/= [vm,erlang,run_queue] ]
       end},
      {"Server state contains callbacks for all Erlang VM metrics",
       fun() ->
               {ok, Callbacks} = application:get_env(exopose, callbacks),
               VMMetrics = exometer:get_values([vm]),
               [ ?assert(proplists:get_value(Name, Callbacks) =/= undefined)
                 || {Name, _DataPoints} <- VMMetrics ]
       end},
      {"New gauge gets added, and its value gets reported",
       fun() ->
               Name = [test,gauge],
               Fun = fun() -> element(3, erlang:now()) end,
               ?assertEqual(ok, exopose:new_gauge(Name, {Fun, []})),
               timer:sleep(200),
               Gauges = exopose:get_gauges(),
               ?assert(existing_metric(Name, Gauges))
       end},
      {"New histogram gets added, and its value gets reported",
       fun () ->
               Name = [test,histogram],
               Fun = fun() -> element(3, erlang:now()) end,
               ?assertEqual(ok, exopose:new_histogram(Name, {Fun, []})),
               timer:sleep(200),
               Histograms = exopose:get_histograms(),
               ?assert(existing_metric(Name, Histograms))
       end},
      {"New counter gets added, and its value gets reported",
       fun () ->
               Name = [test,counter],
               ?assertEqual(ok, exopose:new_counter(Name)),
               timer:sleep(200),
               Counters = exopose:get_counters(),
               ?assert(existing_metric(Name, Counters))
       end},
      {"Attempt to increment non-existing counter, creates it",
       fun () ->
               Name = [test,eunit,non_existing_counter],
               Counters0 = exopose:get_counters(),
               ?assertNot(existing_metric(Name, Counters0)),
               ?assertEqual(ok, exopose:incr(Name)),
               timer:sleep(200),
               Counters1 = exopose:get_counters(),
               ?assert(existing_metric(Name, Counters1))
       end},
      {"Counter names can be parameterized",
       fun () ->
               C1 = [test,result,200],
               C2 = [test,result,404],
               C3 = [test,result,"not_found"],
               ?assertEqual(ok, exopose:new_counter(C1)),
               ?assertEqual(ok, exopose:new_counter(C2)),
               ?assertEqual(ok, exopose:new_counter(C3)),
               timer:sleep(200),
               Counters = exopose:get_counters(),
               [ ?assert(existing_metric(C, Counters)) || C <- [C1,C2,C3] ]
       end}
     ]}.


%%%===================================================================
%%% Helpers
%%%===================================================================

-spec existing_metric(name(), list(any())) -> boolean().
existing_metric(MetricName, ListMetrics) ->
    lists:member(MetricName, [ proplists:get_value(name, M) || M <- ListMetrics ] ).
