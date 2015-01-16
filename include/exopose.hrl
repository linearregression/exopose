%% Metric names
-type name() :: list(atom()).

%% Metric callback
-type callback() :: {mfa() | function(), list(atom())}.

-type metric_type() :: counter | gauge | histogram.
