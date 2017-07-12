-module (practice4).
-export ([start/0, my_monitor/0]).

my_monitor() ->
  spawn(
    fun() ->
      Ref = monitor(process, clock),
      receive
        {'DOWN', Ref, process, _, _} ->
          io:format("stoped.~n"),
          start()
      end
    end
  ).

start() ->
  register(clock, spawn(fun() -> timer(5000) end)).

timer(T) ->
  receive
    stop ->
      exit("Done")
    after T ->
      io:format("I am running.~n"),
      timer(T)
  end.
