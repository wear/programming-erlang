-module (chat).
-export ([start/1, current_time/0]).

start(Broswer) ->
  running(Broswer).

running(Broswer) ->
  receive
    {Broswer, {<<"input">>, Input}} ->
      Msg =  [{<<"cmd">>, <<"speak">>},{<<"body">>, Input}],
      Broswer ! {irc_cmd, Msg},
      running(Broswer);
    {Broswer, {<<"join">>, Input}} ->
      Msg =  [{<<"cmd">>, <<"join">>}, {<<"body">>, Input}],
      Broswer ! {irc_cmd, Msg},
      running(Broswer)
  end.

current_time() ->
  {H,M,S} = time(),
  list_to_binary(io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w", [H,M,S])).

