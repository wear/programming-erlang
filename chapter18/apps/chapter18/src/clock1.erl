-module (clock1).
-export ([start/1, current_time/0]).

start(Broswer) ->
  Broswer ! { update_time, current_time() },
  running(Broswer).

running(Broswer) ->
  receive
    {Broswer, <<"stop">> } ->
      idle(Broswer)
  after 1000 ->
    Broswer ! { update_time, current_time() },
    running(Broswer)
  end.

idle(Broswer) ->
  receive
    {Broswer, <<"start">> } ->
      running(Broswer)
  end.

current_time() ->
  {H,M,S} = time(),
  list_to_binary(io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w", [H,M,S])).

