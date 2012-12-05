Graphic: Nirogen element with Cowboy websocket backend
==============

Graphic is simple-to-use Nitrogen element which allows drawing graphs on web pages using simple data format.

Features
------------
 * Live-updating graphs for realtime data
 * Lazy loading for high-quality zooming

Pre-requisites
-------------
 * [Cowboy](https://github.com/extend/cowboy) - Erlang web server with websocket support
 * [Nitrogen](http://nitrogenproject.com/) - Erlang web framework
 * [SimpleBridge](https://github.com/nitrogen/simple_bridge) - to glue upper two together
 * [HighCharts, HighStocks](http://highcharts.com/) - JS chart library

Setting up
------------
First, you need a working Cowboy+Nitrogen bundle. Then include `highstocks.js` and provided `priv/graphic.js` into your web page. It is enough for drawing static data.
For live updates and background data acquiring you need additional dispatch in Cowboy config.
Typical dispatch option for Cowboy looks like

    {[<<"graphic">>], graphic, []}

... which tells Cowboy to use `graphic` module to handle `/graphic` requests.

Minimal working example can be seen in `src/graphic_demo.erl` file, function `start/0`.



Usage
==========

Basic example: Static data
------------
Simple declaration of graph points. Title option is optional and can be omitted, Graphs are described as tuples:
 * `{Name, Data}` for graphic default (line) graphs
 * `{Name, Options, Data}` for setting any HighCharts.series options (see HighCharts API documentation)

Data is simply list of tuples `{Time, Value}` for most types of graphs.

    body() ->
      #panel{style="width:400px;",
        body = #graphic{data = static_data()}}.

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


Asynchronous data loading
----------------
If your data needs long computations to get, it's better to load it asynchronouly. Graphic uses WebSockets for this.
Just provide `{mfa, Module, Function, Args}` in data field of `#graphic` and define corresponding function.
Let's see an example.

    body() ->
      #panel{style="width:400px;",
        body = #graphic{data = {mfa, ?MODULE, async_static, []}}}.

    async_static() ->
      {ok, static_data(), undefined, stop}.

Return value is 4-tuple `{ok, Data, State, Options}`. To terminate WebSocket connection just after sending data, `Options` is set to `stop`.
`State` is not used in this example, so it may be anything, e.g. `undefined`.


Live updating graph
----------------
Notice `State` and `Options` in return value of `{mfa, ...}` function. You can create `gen_server`-like callbacks to populate already drawn graphic.
Currently there are two options accepted -- `info_handler` and `range_handler`. First one is for specifying message handler. It may be one of these:
 * `Function :: atom()` -- for calling `Function/2` in the same module
 * `{Module :: atom, Function :: atom` -- for calling `Module:Function/2` in any other module
 * Any function of arity 2
 * `{Module, Function, Args}` -- for stateless function called as `Module:Function(Message, Arg1, ...)` using `erlang:apply/3`

For first three options function is called with two arguments -- `(Message, State)` and expected return value is one of these:
 * `{reply, Obj, NewState}` -- sends Obj to client and remembers state
 * `{reply, Obj}` -- same as above but keeps old state
 * `noreply`, `{noreply, NewState}` -- no data sent to client, new state is optionally set
 * `stop`, `{stop, Obj}` -- terminate WebSocket after optional data is sent

Returned `Obj` is in minimal data format -- `[{Name, [{Time, Value}]}]`

    body() ->
      #panel{style="width:400px;",
        body = #graphic{data = {mfa, ?MODULE, random_data, [1000]}}}.

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


Dynamic detalization graph
------------------
Geaphic supports dynamic detalization. When you zoom such graphic in, client makes request to server for data on selected interval. This allows you save bandwidth and keep client fast even on large data set. Unfortunately, dynamic detalization in incompatible with live updates.

For example, we have a function of time:

    time_fun(UTC) ->
      Minutes = (UTC rem timer:hours(1)) / timer:minutes(1),
      Seconds = (UTC rem timer:minutes(1)) / timer:seconds(1),
      % Some high-freq function modulated by low-freq one
      10*(1 + math:sin(Minutes/5)) * math:sin(Seconds).

We show some points by default and specify range handler:

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

When user sets new range on graphic, callback is called and graphic data is replaced by returned data:

    time_detail(Request, Count) ->
      From = proplists:get_value(min, Request),
      To = proplists:get_value(max, Request),
      Step = erlang:max(1, (To - From) div Count),
    
      Points = [{T, time_fun(T)} || T <- lists:seq(From, To, Step)],
      {reply, [{nice_fun, Points}]}.
