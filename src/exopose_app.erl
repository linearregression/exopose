-module(exopose_app).

-behaviour(application).

%% API
-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

-define(GET_ENV(K),     application:get_env(K)).
-define(GET_ENV(A,K),   application:get_env(A,K)).
-define(SET_ENV(A,K,V), application:set_env(A,K,V)).

-define(GET_VAL(K,L), proplists:get_value(K,L)).

%% ===================================================================
%% API
%% ===================================================================

start() ->
    {ok, _} = application:ensure_all_started(exopose).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, _} = start_exometer_core(),
    exopose_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% `exometer_core` may be configured with statically defined entries,
%% which are defined under variable `predefined`.
%% `exopose` is provided with a default set of entries to monitor
%% the Erlang VM (see `exopose.app.src`). Those are passed to
%% `exometer_core` environment in the function below unless they have
%% been overwriten by an external config file.
start_exometer_core() ->
    {ok, ExometerConfig} = ?GET_ENV(exometer_core),
    application:load(exometer_core),
    case ?GET_ENV(exometer_core, predefined) of
        undefined ->
            ok = ?SET_ENV(exometer_core, predefined, ?GET_VAL(predefined, ExometerConfig));
        {ok, _Val} ->
            ok
    end,
    ok = application:start(exometer_core),
    {ok, started}.
