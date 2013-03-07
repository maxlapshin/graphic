-module(graphic_quotes_demo).

-include_lib("graphic/include/graphic.hrl").

-export([body/1]).
-export([graphic/1, handle_info/2]).

body(Stock) ->
  % Configure fix
  application:start(fix),
  {ok, FixConf} = file:consult("fix_src.conf"),
  application:set_env(fix, fix_src, FixConf),

  #graphic{style="width:600px;", data = {mfa, ?MODULE, graphic, [Stock]}}.

% Graphic callback
graphic(Stock) ->
  Config = stored_graphs(Stock) ++ config_options(Stock),
  {State0, Options} = runtime_params(Stock),
  {ok, Config, State0, Options}.

% Return graphs from stockdb
stored_graphs(Stock) ->
  {Date, _} = calendar:universal_time(),
  Events = stored_data(Stock, Date),
  unpack(Events, [], [], []).

% Graphic configuration
config_options(Stock) ->
  Range = timer:minutes(10),
  [ {option, range, Range},
    {option, title, Stock},
    {option, navigator, true},
    {option, ordinal, false},
    {option, legend, true} ].

runtime_params(Stock) ->
  case fix_reader:ensure_saving(fix_src, Stock) of
    {ok, StockPid} ->
      % Subscribe to stock updates
      graphic_fix_updater:attach(StockPid),
      % flush collected events periodically
      timer:send_interval(timer:seconds(5), flush),
      {rev, [{info_handler, handle_info}]};
    _ ->
      % Stock is not live -- no updates
      {undefined, stop}
  end.

% Get raw stockdb data
stored_data(Stock, Date) ->
  Period = timer:minutes(1),
  stockdb:events(Stock, Date, [
      {filter, candle, [{type, md}, {period, Period}]},
      {filter, candle, [{type, trade}, {period, Period}]}
    ]).


% Incoming message handler
handle_info({Type, _, _, _} = Evt, Acc) when Type == md; Type == trade ->
  {noreply, [Evt|Acc]};

handle_info(flush, rev) ->
  {noreply, rev};

handle_info(flush, Acc) ->
  Unpacked = unpack(Acc, [], [], []),
  Update = [{Name, hlc(Data)} || {Name, Data} <- Unpacked, Data /= []],
  {reply, Update, rev};

handle_info({'EXIT', _, _}, Acc) ->
  Unpacked = unpack(Acc, [], [], []),
  Update = [{Name, hlc(Data)} || {Name, Data} <- Unpacked, Data /= []],
  {stop, Update}.



% Unpack stockdb-style events into graphs
unpack([{md, UTC, [{Bid1, _}|_], [{Ask1, _}|_]}|Events], RBid, RAsk, RTrade) ->
  unpack(Events, [{UTC, Bid1}|RBid], [{UTC, Ask1}|RAsk], RTrade);

unpack([{trade, UTC, Trade, _Vol}|Events], RBid, RAsk, RTrade) ->
  unpack(Events, RBid, RAsk, [{UTC, Trade}|RTrade]);

unpack([], RBid, RAsk, RTrade) ->
  unpack(rev, lists:reverse(RBid), lists:reverse(RAsk), lists:reverse(RTrade));

unpack(rev, Bid, Ask, Trade) ->
  [{bid, Bid}, {ask, Ask}, {trade, Trade}].


% Simplify collected data to not overload browser
hlc([]) -> [];
hlc([Hd|Events]) ->
  hlc(Events, Hd, Hd).

hlc([Close], High, Low) ->
  lists:usort([vmax(High, Close), vmin(Low, Close), Close]);
hlc([E|Events], High, Low) ->
  hlc(Events, vmax(High, E), vmin(Low, E)).

vmax({UTC, A}, {_, B}) when A >= B -> {UTC, A};
vmax({_, _}, {UTC, B}) -> {UTC, B}.

vmin({UTC, A}, {_, B}) when A =< B -> {UTC, A};
vmin({_, _}, {UTC, B}) -> {UTC, B}.

