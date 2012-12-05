-module(element_graphic).
-author({"Danil Zagoskin", z@gosk.in}).

-include_lib("nitrogen_core/include/wf.hrl").
-include("../include/graphic.hrl").

% Typical element exports
-export([render_element/1, reflect/0]).

-export([make_graphic_config/1]).

% For internal Nitrogen use
reflect() -> record_info(fields, graphic).

render_element(#graphic{client_id = undefined} = E) ->
  ID_ = io_lib:format("graphic_~s", [wf:temp_id()]),
  ID = erlang:iolist_to_binary(ID_),
  render_element(E#graphic{client_id = ID});

render_element(#graphic{client_id = ID, ratio = Ratio} = E) ->
  JSElt = io_lib:format("$('#~s')[0]", [ID]),

  wf:wire(io_lib:format("Graphic.autoHeight(~s, ~w);", [JSElt, Ratio])),
  wire_data(E),

  wf_tags:emit_tag('div', [], [
      {id, ID},
      {class, [simple_graph, E#graphic.class]},
      {style, E#graphic.style}
    ]).

wire_data(#graphic{data = Data} = E) ->
  wire_prepared_data(E, make_graphic_config(Data)).


make_graphic_config(Data) ->
  case prepare_data(Data) of
    {mfa, _, _, _} = MFA ->
      MFA;
    Prepared ->
      Options = [{Key, Value} || {option, Key, Value} <- Prepared],
      GraphData = [[{name,Name},{data,GData}|GOptions] || {graph, Name, GOptions, GData} <- Prepared],
      {tag_proplists(Options), tag_proplists(GraphData)}
  end.

tag_proplists([{_,_}|_] = PL) ->
  {struct, [{Key, tag_proplists(Value)} || {Key, Value} <- PL]};
tag_proplists([[{_,_}|_]|_] = List) ->
  [tag_proplists(E) || E <- List];
tag_proplists(Term) -> Term.

% MFA graph: dummy for special wire action
prepare_data({mfa, Module, Function, Args}) ->
  {mfa, Module, Function, Args};

% Option: It will go to special object
prepare_data([{option, Key, Value}|MoreData]) ->
  [{option, Key, Value}|prepare_data(MoreData)];

prepare_data([{options, Options}|MoreData]) ->
  PreparedOptions = [{option, Key, Value} || {Key, Value} <- Options],
  PreparedOptions ++ prepare_data(MoreData);

% Insert empty options list if no options given
prepare_data([{Name, Points}|MoreData]) ->
  prepare_data([{Name, [], Points}|MoreData]);

% Empty graph (for later updates)
prepare_data([{Name, Options, []}|MoreData]) ->
  [{graph, Name, Options, []}|prepare_data(MoreData)];
% Scalar numeric data
prepare_data([{Name, Options, [Y|_] = Points}|MoreData]) when is_number(Y) ->
  [{graph, Name, Options, prepare_y(Points)}|prepare_data(MoreData)];
% (x,y) or (utc,y) data
prepare_data([{Name, Options, [{X,Y}|_] = Points}|MoreData]) when is_number(X) andalso is_number(Y) ->
  [{graph, Name, Options, prepare_xy(Points)}|prepare_data(MoreData)];

% End of data
prepare_data([]) -> [].


prepare_y(Points) ->
  % Nothing special. Just validate
  lists:map(fun(Y) when is_number(Y) -> Y end, Points).

prepare_xy(Points) ->
  % Repack tuples into lists, validate values
  lists:map(fun({X,Y}) when is_number(X) andalso is_number(Y) ->
        [X,Y]
    end, Points).


wire_prepared_data(#graphic{client_id = ID}, {mfa, Module, Function, Args}) ->
  PickledMFA = graphic:pickle({Module, Function, Args}),
  Script = lists:flatten(io_lib:format("Graphic.ws_request('~s', '~s');", [ID, PickledMFA])),
  wf:wire(Script);

wire_prepared_data(#graphic{client_id = ID}, {Options, GraphData}) ->
  OptionsJSON = mochijson2:encode(Options),
  DataJSON = mochijson2:encode(GraphData),

  Script = lists:flatten(io_lib:format("Graphic.render('~s', ~s, ~s);", [ID, OptionsJSON, DataJSON])),
  wf:wire(Script).

