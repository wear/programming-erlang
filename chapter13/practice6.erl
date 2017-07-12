-module (practice6).
-export ([start/1]).
% c(practice6).
% practice6:start([p1,p2]).

start(Terms) ->
  spawn(
    fun() ->
      L = [{spawn_monitor(fun() -> timer(Term) end), Term} || Term <- Terms],
      [register(T, Pid) || {{Pid, _}, T} <- L],
      receive
        {'DOWN', _, process, _, Atom} ->
          io:format("~p stop ~n", [Atom]),
          [exit(Pid, kill) || {{Pid, _}, T} <- L, Atom =/= T],
          start(Terms)
      end
    end
  ).

timer(T) ->
  receive
    stop ->
      exit(T)
    after 3000 ->
      io:format("I am running ~p~n", [T]),
      timer(T)
  end.


