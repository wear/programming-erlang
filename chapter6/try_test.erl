-module (try_test).
-export ([demo1/0]).
-vsn(1234).
-auther("Stephen Kong").

demo1() ->
  [catcher(N) || N <- [1,2,3,4,5]].

catcher(N) ->
  try general_exceptin(N) of
    Val -> {N, normal, Val}
  catch
    throw:X ->
      io:format("Error, ~p not supported.", [X]),
      {N, erlang:get_stacktrace()};
    exit:X -> {N, caught, exited, X};
    error:X -> {N, caught, error, X}
  end.


general_exceptin(1) -> a;
general_exceptin(2) -> throw(a);
general_exceptin(3) -> exit(a);
general_exceptin(4) -> {'EXIT', a};
general_exceptin(5) -> error(a).