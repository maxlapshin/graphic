-module(graphic_demo).

% User API
-export([start/0]).

% Cowboy handler API
-export([init/3, handle/2, terminate/2]).

% Nitrogen callbacks
-include("../include/graphic.hrl").
-export([main/0, body/0]).

% Demo graphic callbacks
-export([async_static/0, random_data/1, time_graphic/0, time_detail/2, la_graphic/1, update_la/2]).


start() ->
  %% Start Cowboy...
  application:start(mimetypes),
  application:start(cowboy),

  JSStatic = {[<<"js">>, '...'], cowboy_http_static, [{directory, code:lib_dir(graphic, priv)}]},
  NitrogenStatic = {[<<"nitrogen">>, '...'], cowboy_http_static, [{directory, code:lib_dir(nitrogen_core, www)}]},
  GraphicDispatch = {[<<"graphic">>], graphic, []},
  NitrogenDispatch = {[<<"graphic_demo">>], ?MODULE, []},

  Dispatch = [{'_', [JSStatic, NitrogenStatic, GraphicDispatch, NitrogenDispatch]}],
  HttpOpts = [{max_keepalive, 50}, {dispatch, Dispatch}],

  cowboy:start_listener(http, 100,
    cowboy_tcp_transport, [{port, 8880}],
    cowboy_http_protocol, HttpOpts),

  ok.

% Cowboy -> Nitrogen adapter
-record(state, {headers, body}).

init({_Transport, http}, Req, Opts) ->
  Headers = proplists:get_value(headers, Opts, []),
  Body = proplists:get_value(body, Opts, "http_handler"),
  {ok, Req, #state{headers=Headers, body=Body}}.

handle(Req, Opts) ->
  RequestBridge = simple_bridge:make_request(cowboy_request_bridge, {Req, "./priv"}),
  ResponseBridge = simple_bridge:make_response(cowboy_response_bridge, RequestBridge),

  nitrogen:init_request(RequestBridge, ResponseBridge),

  {ok, NewReq} = nitrogen:run(),
  {ok, NewReq, Opts}.

terminate(_Req, _State) ->
  ok.


% Nitrogen page
main() -> #template{file = filename:join([code:lib_dir(graphic, priv), "demo.tmpl"])}.
body() ->
  #panel{style="width:100%; display:table;", body = [
      #panel{style="display:table-row;", body = [
          #panel{style="width:400px; display:table-cell;",
            body = #graphic{client_id = static, data = static_data()}},

          #panel{style="width:400px; display:table-cell;",
            body = #graphic{client_id = static_async, data = {mfa, ?MODULE, async_static, []}}},

          #panel{style="width:400px; display:table-cell;",
            body = #graphic{client_id = line_demo, data = static_lines()}},

          #panel{}
        ]},
      #panel{style="display:table-row;", body = [
          #panel{style="width:400px; display:table-cell;",
            body = #graphic{client_id = random, data = {mfa, ?MODULE, random_data, [1000]}} },

          #panel{style="width:400px; display:table-cell;",
            body = #graphic{client_id = loadavg, data = {mfa, ?MODULE, la_graphic, [200]}} },

          #panel{style="width:400px; display:table-cell;",
            body = #graphic{client_id = modulated, data = {mfa, ?MODULE, time_graphic, []}} },

          #panel{}
        ]},
      #panel{style="display:table-row;", body = [
          #panel{style="width:400px; display:table-cell;",
            body = #graphic{client_id = chart_backend, data = chart_backend()}},

          #panel{}
        ]}
    ]}.


% Demo graphics
static_data() ->
  [{option, title, <<"Static">>},
    {graph1, [
        {1354723700000, 17},
        {1354723710000, 19},
        {1354723720000, 20},
        {1354723730000, 22},
        {1354723740000, 18},
        {1354723750000, 17} ]},
    {graph2, [{type, scatter}], [
        {1354723703000, 20},
        {1354723713000, 22},
        {1354723723000, 19},
        {1354723733000, 19},
        {1354723743000, 21},
        {1354723753000, 20} ]}
  ].

async_static() ->
  {ok, static_data(), undefined, stop}.

static_lines() ->
  [{option, title, <<"Line demo">>},
    {option, lines, [
        {line1, 14},
        {line2, [19, red, right]},
        {line3, [25, left, long_dash_dot_dot, green, 3]} ]},
    {some_graph, [
        {1354723700000, 13},
        {1354723710000, 29},
        {1354723720000, 20} ]}
  ].

random_data(Interval) ->
  Config = [
    {option, title, <<"Random">>},
    {option, navigator, true},
    {random, []} ],
  timer:send_interval(Interval, random),
  {ok, Config, undefined, [{info_handler, fun random_sender/1}]}.

random_sender(random) ->
  Point = {now_ms(), random:uniform()},
  {reply, [{random, [Point]}]}.


time_graphic() ->
  Range = erlang:round(timer:hours(1.1)),
  Count = 300,

  Now = now_ms(),
  Start = Now - Range,
  Step = Range div Count,
  
  Times = lists:seq(Start, Now, Step),

  Config = [
    {option, navigator, nice_fun},
    {option, title, <<"Zoomable">>},
    {nice_fun, [{T, time_fun(T)} || T <- Times]} ],
  {ok, Config, undefined, [{range_handler, {?MODULE, time_detail, [Count]}}]}.

time_detail(Request, Count) ->
  From = proplists:get_value(min, Request),
  To = proplists:get_value(max, Request),
  Step = erlang:max(1, (To - From) div Count),

  Points = [{T, time_fun(T)} || T <- lists:seq(From, To, Step)],
  {reply, [{nice_fun, Points}]}.


time_fun(UTC) ->
  Minutes = (UTC rem timer:hours(1)) / timer:minutes(1),
  Seconds = (UTC rem timer:minutes(1)) / timer:seconds(1),
  % Some high-freq function modulated by low-freq one
  10*(1 + math:sin(Minutes/5)) * math:sin(Seconds).


la_graphic(Interval) ->
  case lists:keymember(sasl, 3, application:which_applications()) of
    true -> ok;
    false ->
      application:load(sasl),
      application:set_env(sasl, errlog_type, error),
      application:start(sasl)
  end,
  application:start(os_mon),

  Config = [
    {option, navigator, la1},
    {option, legend, true},
    {option, title, <<"Load averages">>},
    {la1, []},
    {la5, []},
    {la15, []} ],
  timer:send_interval(Interval, update_la),
  {ok, Config, undefined, [{info_handler, update_la}]}.

update_la(update_la, _State) ->
  LA1 = cpu_sup:avg1()/256,
  LA5 = cpu_sup:avg5()/256,
  LA15 = cpu_sup:avg15()/256,
  Now = now_ms(),
  Update = [
    {la1, [{Now, LA1}]},
    {la5, [{Now, LA5}]},
    {la15, [{Now, LA15}]} ],
  {reply, Update}.


now_ms() ->
  now_to_ms(now()).
now_to_ms({MegaSec, Sec, Microsec}) ->
  Microsec div 1000 + (Sec + 1000000*MegaSec) * 1000.



chart_backend() ->
  [{option, title, <<"Chart backend">>},
    {option, backend, chart},
    {graph1, [
        {0.7, 22},
        {1.0, 18},
        {1.1, 17} ]}
  ].
