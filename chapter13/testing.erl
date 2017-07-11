-module (testing).
-export ([start/0]).

start() ->
  receive
    gone ->
      exit("gone")
  end.
