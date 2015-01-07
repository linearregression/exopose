%% Metric names
-type name() :: list(atom()).

%% Metric callback
-type callback() :: {mfa() | function(), list(atom())}.

%% Metric types
-type gauge() :: [{'name', binary(),
                   'value', integer(),
                   'ms_since_reset', pos_integer()}].
-type counter() :: [{'name', binary(),
                     'value', integer(),
                     'ms_since_reset', pos_integer()}].
