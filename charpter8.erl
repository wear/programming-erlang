-module (charpter8).
-compile(export_all).

max_func_list_count() ->
  L = lists:sort(fun({_,A},{_,B}) -> length(A) < length(B) end, func_list()),
  lists:last(L).

most_frequncy_func() ->
  L = [Func || {_, FuncList} <- func_list(), {Func, _} <- FuncList],

  T = lists:foldl(
    fun(Func, [{Func, Count}|T]) ->
      [{Func,Count+1}|T];
      (Func, Acc) ->
        [{Func, 1}|Acc]
    end,
    [],
    L
  ),
  lists:last(
    lists:sort(fun({_,A},{_,B}) -> A < B end, T)
  ).

func_list() ->
  L = code:all_loaded(),
  lists:map(
    fun({Mod, _File}) ->
      {Mod, [Func || {exports, FuncList} <- Mod:module_info(), Func <- FuncList]}
    end,
    L
  ).

get_mod_exports(Mod) ->
  lists:filter(fun({Attr, _}) -> Attr =:= exports end, Mod:module_info()).

