-module(graphic_fix_updater).

-export([attach/1]).
-export([init/1, handle_info/2, handle_event/2, terminate/2]).

attach(Stock) ->
  Graphic = self(),
  gen_event:add_sup_handler(Stock, {?MODULE, Graphic}, Graphic).

init(Graphic) ->
  {ok, Graphic}.

handle_info(Event, Graphic) ->
  maybe_proxy(Event, Graphic).

handle_event(Event, Graphic) ->
  maybe_proxy(Event, Graphic).

terminate(_, _) -> ok.


maybe_proxy(Event, Graphic) ->
  % Convert to stockdb events if possible
  ToSend = case fix_market_data:to_stockdb(Event, []) of
    undefined -> [];
    SDBEvents -> SDBEvents
  end,
  % Send stockdb events
  [Graphic ! E || E <- ToSend],
  {ok, Graphic}.
