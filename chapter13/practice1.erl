-module (practice1).
-export ([own_spawn/3, own_spawn/4]).

% chapter13:own_spawn(testing, start, []).

own_spawn(Mod, Func, Args) ->
  Pid = spawn(Mod, Func, Args),
  T1 = erlang:timestamp(),
  spawn(
    fun() ->
      Ref = monitor(process, Pid),
      receive
        {'DOWN', Ref, process, Pid, Why} ->
          T2 = erlang:timestamp(),
          T = timer:now_diff(T2, T1) / 1000000,
          io:format("Down because of ~p, alive ~p seconds.~n", [Why,T])
      end
    end
  ),
  Pid.

% chapter13:own_spawn(testing, start, [], 3000).

own_spawn(Mod, Func, Args, Time) ->
  Pid = spawn(Mod, Func, Args),
  spawn(
    fun() ->
      timer:exit_after(Time, Pid, "Timeup"),
      Ref = monitor(process, Pid),
      receive
        {'DOWN', Ref, process, Pid, Why} ->
          io:format("Down because of ~p~n", [Why])
      end
    end
  ),
  Pid.