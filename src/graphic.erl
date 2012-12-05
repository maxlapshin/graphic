-module(graphic).
-author({"Danil Zagoskin", z@gosk.in}).

-include_lib("cowboy/include/http.hrl").
-include("../include/graphic.hrl").

-export([init/3, websocket_init/3]).
-export([websocket_handle/3, websocket_info/3, websocket_terminate/3]).

-export([pickle/1, depickle/1]).


-record(state, {
    mfa,
    handler_state,
    info_handler,
    ws_handler
  }).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public pickle api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pickle(Term) ->
  Bin = erlang:term_to_binary(Term, [compressed]),
  _Encoded = base64:encode(Bin).

depickle(Encoded) ->
  Bin = base64:decode(Encoded),
  erlang:binary_to_term(Bin).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cowboy callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_http_websocket}.

websocket_init(_Transport, Req, _Opts) ->
  {ok, Req, undefined}.


% Initial packet. Read MFA and parse result
websocket_handle({text, Request}, Req, undefined) ->
  {struct, Obj} = nitro_mochijson2:decode(Request),
  {Module, Function, Args} = depickle(proplists:get_value(<<"mfa">>, Obj)),
  {ok, Config, HState, Options} = erlang:apply(Module, Function, Args),
  State0 = #state{
    mfa = {Module, Function, Args},
    handler_state = HState,
    info_handler = proplists:get_value(info_handler, Options),
    ws_handler = proplists:get_value(ws_handler, Options)
  },
  {GOptions, GData} = element_graphic:make_graphic_config(Config),
  InitBody = nitro_mochijson2:encode({struct, [{init, true}, {options, GOptions}, {data, GData}]}),
  {reply, {text, InitBody}, Req, State0};

websocket_handle({text, Packet}, Req, #state{ws_handler = Handler} = State) when Handler /= undefined ->
  {struct, Event} = nitro_mochijson2:decode(Packet),
  apply_handler(Handler, Event, Req, State).


% When handler is undefined proxy messages in {pass, Msg} format
websocket_info({pass, Message}, Req, #state{info_handler = undefined} = State) ->
  Body = nitro_mochijson2:encode({struct, Message}),
  {reply, {text, Body}, Req, State};

% Magic 'shutdown' message
websocket_info(shutdown, Req, #state{info_handler = undefined} = State) ->
  {shutdown, Req, State};

% Ignore other messages when handler is undefined
websocket_info(_, Req, #state{info_handler = undefined} = State) ->
  {ok, Req, State};

% Apply specified handler
websocket_info(Message, Req, #state{info_handler = Handler} = State) ->
  apply_handler(Handler, Message, Req, State).

% Dummy terminate
websocket_terminate(_Reason, _Req, _State) ->
  ok.

% Stateless handler - Just apply
apply_handler({Module, Function, Args}, Message, Req, State) ->
  Result = erlang:apply(Module, Function, [Message|Args]),
  case Result of
    undefined ->
      {ok, Req, State};
    {ok, Reply} ->
      Body = nitro_mochijson2:encode({struct, Reply}),
      {reply, {text, Body}, Req, State}
  end;

% handle_info style callback
apply_handler(Handler, Message, Req, #state{handler_state = HState} = State) when is_function(Handler, 2) ->
  Result = Handler(Message, HState),
  case Result of
    {reply, Reply} ->
      Body = nitro_mochijson2:encode({struct, Reply}),
      {reply, {text, Body}, Req, State};
    {reply, Reply, NewHState} ->
      Body = nitro_mochijson2:encode({struct, Reply}),
      {reply, {text, Body}, Req, State#state{handler_state = NewHState}};
    noreply ->
      {ok, Req, State};
    {noreply, NewHState} ->
      {ok, Req, State#state{handler_state = NewHState}};
    stop ->
      {shutdown, Req, State};
    {stop, Reply} ->
      self() ! shutdown,
      Body = nitro_mochijson2:encode({struct, Reply}),
      {reply, {text, Body}, Req, State#state{info_handler = undefined}}
  end;

apply_handler({Module, Function}, Message, Req, #state{} = State) ->
  apply_handler(fun Module:Function/2, Message, Req, State);

apply_handler(Function, Message, Req, #state{mfa = {Module, _, _}} = State) when is_atom(Function) ->
  apply_handler(fun Module:Function/2, Message, Req, State).


