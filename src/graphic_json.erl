-module(graphic_json).
-author({"Danil Zagoskin", z@gosk.in}).

-export([encode/1, decode/1]).

encode(Term) ->
  Tagged = tag_proplists(Term),
  nitro_mochijson2:encode(Tagged).

decode(JSON) ->
  Decoded = nitro_mochijson2:decode(JSON),
  untag_structs(Decoded).


tag_proplists([{_,_}|_] = PL) ->
  {struct, [{Key, tag_proplists(Value)} || {Key, Value} <- PL]};
tag_proplists([[{_,_}|_]|_] = List) ->
  [tag_proplists(E) || E <- List];
tag_proplists(Term) -> Term.


untag_structs({struct, PL}) ->
  [{parse_key(Key), untag_structs(Value)} || {Key, Value} <- PL];
untag_structs([{struct, _}|_] = SList) ->
  [untag_structs(S) || S <- SList];
untag_structs(Term) -> Term.

parse_key(Key) ->
  case re:run(Key, "^[0-9]*(|\\.[0-9]*)$") of
    {match, [_, {_, 0}]} -> % integer
      List = erlang:binary_to_list(<<"0", Key/binary>>),
      erlang:list_to_integer(List);
    {match, [_, _]} -> % float
      List = erlang:binary_to_list(<<"0", Key/binary, "0">>),
      erlang:list_to_float(List);
    nomatch ->
      erlang:binary_to_atom(Key, latin1)
  end.
