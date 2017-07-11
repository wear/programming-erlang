-module (my_tuple_to_list).
-export ([cover/1, start/0]).

start() ->
  io:format("started").

cover([]) -> [];
cover(Tuple) ->
  [element(I,Tuple) || I <- lists:seq(1,tuple_size(Tuple))].

test() ->
  [a, b, c] = cover({a, b, c}),
  test_worked.