-module(lyr_metrics_app).

-behaviour(application).

%% API
-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

-define(SET_ENV(K,V,E), application:set_env(K,V,E)).
-define(GET_VAL(K,L), proplists:get_value(K,L)).
-define(DELETE(K,L), proplists:delete(K, L)).

%% ===================================================================
%% API
%% ===================================================================

start() ->
    {ok, _} = application:ensure_all_started(lyr_metrics, transient).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, _} = start_custom_reporter(),
    lyr_metrics_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% exometer needs to be start with custom environment
%% variables that depend on the host it runs at, for which
%% we modify the given config to start exometer with.
%% The reporter of choice is collectd exometer built-in backend.
start_custom_reporter() ->
    {ok, ExometerConfig} = application:get_env(exometer),
    ok = application:load(exometer),
    case ?GET_VAL(report, ExometerConfig) of
        undefined ->
            ok = application:unload(exometer),
            {ok, missing_config};
        Report ->
            Subscribers    = ?GET_VAL(subscribers, Report),
            Reporters      = ?GET_VAL(reporters, Report),
            ReportCollectd = ?GET_VAL(exometer_report_collectd, Reporters),
            Path           = ?GET_VAL(path, ReportCollectd),
            ConnectTimeout = ?GET_VAL(connect_timeout, ReportCollectd),
            case is_collectd_socket_ready(Path, ConnectTimeout) of
                true ->
                    NewHostname =
                        case os:getenv("PARENT_HOSTNAME") of
                            false ->
                                atom_to_list(node());
                            Env ->
                                Env
                        end,
                    NewReportCollectd = [{hostname, NewHostname} | ?DELETE(hostname, ReportCollectd)],
                    NewReporters = [{exometer_report_collectd, NewReportCollectd}],
                    NewReport = [{reporters, NewReporters}, {subscribers, Subscribers}],
                    ok = ?SET_ENV(exometer, report, NewReport),
                    ok = ?SET_ENV(exometer, predefined, ?GET_VAL(predefined, ExometerConfig)),
                    ok = application:start(exometer),
                    {ok, started};
                false ->
                    {ok, cannot_open_socket}
            end
    end.

%% Tests whether there's a collectd unix socket read at the provided path.
is_collectd_socket_ready(Path, Timeout) ->
    case afunix:connect(Path, [{active, false}, {mode, binary}], Timeout) of
        {ok, _Socket} -> true;
        {error, _Reason} -> false
    end.
