-module(exopose_tests).

-include_lib("eunit/include/eunit.hrl").

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
               [ ?assert(Val > 0) || {Name, Val} <- VMMetrics, Name =/= <<"vm_erlang_run_queue">> ]
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
               CurrentGauges = length(exopose:get_gauges()),
               Name = [test,gauge],
               Fun = fun() -> element(3, erlang:now()) end,
               ?assertEqual(ok, exopose:new_gauge(Name, {Fun, []})),
               timer:sleep(200),
               ?assertEqual(CurrentGauges + 1, length(exopose:get_gauges()))
       end},
      {"New histogram gets added, and its value gets reported",
       fun () ->
               CurrentHistograms = length(exopose:get_histograms()),
               Fun = fun() -> element(3, erlang:now()) end,
               ?assertEqual(ok, exopose:new_histogram([test,histogram], {Fun, []})),
               timer:sleep(200),
               ?assertEqual(CurrentHistograms + 1, length(exopose:get_histograms()))
       end},
      {"New counter gets added, and its value gets reported",
       fun () ->
               CurrentCounters = length(exopose:get_counters()),
               ?assertEqual(ok, exopose:new_counter([test,counter])),
               timer:sleep(200),
               ?assertEqual(CurrentCounters + 1, length(exopose:get_counters()))
       end},
      {"Attempt to increment non-existing counter, creates it",
       fun () ->
               CurrentCounters = length(exopose:get_counters()),
               ?assertEqual(ok, exopose:incr([test,eunit,counter])),
               ?assertEqual(CurrentCounters + 1, length(exopose:get_counters()))
       end}
     ]}.
