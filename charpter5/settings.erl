-module (settings).
-export ([read/1, map_search_pred/2, map_delete_pred/2]).

% "./config.json"
read(File) ->
  {ok, Json} = file:read_file(File),
  binary_to_list(Json).

map_search_pred(Map, Pred) ->
  map_list_search_pred(maps:to_list(Map), Pred).

map_list_search_pred([{Key, Value}|T], Func) ->
  case Func(Key, Value) of
    true -> {Key, Value};
    fase -> map_list_search_pred(T, Func)
  end.

% settings:map_search_pred(#{a => "a", b => "a"}, fun(Key, _Value) -> Key =:= a end)

% delete_if {| key, value | block } → hsh

map_delete_pred(Map, Pred) ->
  L = maps:to_list(Map),
  map_list_delete_pred(Map, L, Pred).

map_list_delete_pred(Map, [], _Func) ->
  Map;
map_list_delete_pred(Map, [{Key, Value}|T], Func) ->
  case Func(Key, Value) of
    true ->
      maps:remove(Key, Map);
    false ->
      map_list_delete_pred(Map, T, Func)
  end.

% settings:map_delete_pred(#{a => "a", b => "b"}, fun(Key, _Value) -> Key =:= ac end)

