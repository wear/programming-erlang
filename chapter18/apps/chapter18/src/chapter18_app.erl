%%%-------------------------------------------------------------------
%% @doc chapter18 public API
%% @end
%%%-------------------------------------------------------------------

-module(chapter18_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {priv_file, chapter18, "index.html"}},
      {"/clock", ws_handler, []},
      {"/static/[...]", cowboy_static, {priv_dir, chapter18, "static"}}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
    env => #{dispatch => Dispatch}
  }),
  chapter18_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
