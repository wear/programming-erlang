-module (ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, State) ->
  {cowboy_websocket, Req, State, #{idle_timeout => infinity}}.

websocket_init(State) ->
  Mod = maps:get(mod, State),
  Pid = spawn(Mod, start, [self()]),
  NS = State#{pid => Pid},
  {ok, NS}.

websocket_handle({_, Msg}, State) ->
  Pid = maps:get(pid, State),
  [Cmd] = jsx:decode(Msg),
  io:format("CMD is ~p~n", [Cmd]),
  Pid ! {self(), Cmd},
  {ok, State}.

websocket_info({irc_cmd, Msg}, State) ->
  io:format("Msg is ~p~n",[jsx:encode(Msg)]),
  {reply, {text, jsx:encode(Msg)}, State};
websocket_info({update_time, Msg}, State) ->
  {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
  {ok, State}.


