-module (practice4).
-export ([running/0]).

running() ->
  receive
    after 5000 ->
      io:format("I am running.~n"),
      running()
  end.