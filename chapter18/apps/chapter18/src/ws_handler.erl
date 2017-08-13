-module(ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, State) ->
  {cowboy_websocket, Req, State, #{idle_timeout => infinity}}.

websocket_init(State) ->
  Pid = spawn(clock1, start, [self()]),
  NS = #{clock => Pid},
  {ok, NS}.

websocket_handle(Data, State) ->
  Pid = maps:get(clock, State),
  {_, Msg} = Data,
  Pid ! {self(), Msg},
  {ok, State}.

websocket_info({update_time, Msg}, State) ->
  % Pid = maps:get(clock, State),
  {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
  {ok, State}.
