-module(exopose_util).

-include("exopose.hrl").

-export([pp/1]).

%%%===================================================================
%%% Exported functions
%%%===================================================================

%% @doc Pretty printing of exometer's metric names
-spec pp(name()) -> string().
pp(Name) when is_list(Name) ->
    pp(Name, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

pp([], Acc) ->
    lists:flatten(lists:reverse(Acc));

pp([Atom], Acc) when is_atom(Atom) ->
    pp([], [atom_to_list(Atom) | Acc]);
pp([Int], Acc) when is_integer(Int) ->
    pp([], [integer_to_list(Int) | Acc]);
pp([String], Acc) when is_list(String) ->
    pp([], [String | Acc]);

pp([Atom | Rest], Acc) when is_atom(Atom) ->
    pp(Rest, ["_", atom_to_list(Atom) | Acc]);
pp([Int | Rest], Acc) when is_integer(Int) ->
    pp(Rest, ["_", integer_to_list(Int) | Acc]);
pp([String | Rest], Acc) when is_list(String) ->
    pp(Rest, ["_", String | Acc]).

%%%===================================================================
%%% EUnit
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

pp_test() ->
    ?assertEqual("", pp([])),
    ?assertEqual("random_name", pp([random_name])),
    ?assertEqual("first_second", pp([first, second])),
    ?assertEqual("a_b_c", pp([a,b,c])),
    ?assertEqual("test_function_returns_200", pp([test,function,returns,200])).

-endif.
