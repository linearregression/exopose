-module(exopose_tests).

-include_lib("eunit/include/eunit.hrl").

exopose_server_test_() ->
    {setup,
     fun() ->
             {ok, Started} = application:ensure_all_started(exopose),
             Started
     end,
     fun(Started) ->
             [ ok = application:stop(App) || App <- lists:reverse(Started), App =/= lager ]
     end,
     [
      {"Server starts with pre-configured Erlang VM metrics",
       fun() ->
               ?assert(length(exopose:vm()) > 0)
       end}
     ]}.
