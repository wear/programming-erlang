-module (my_time).
-export ([my_time_func/1, test/0]).

my_time_func(F) ->
  F().

test() ->
  {Hour, Min, _} = time(),
  my_time_func(fun() -> io:format("datetime is ~p:~p~n", [Hour, Min]) end),
  test_worked.
