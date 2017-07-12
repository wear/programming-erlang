-module (practice5).
-export ([start/1]).

% practice5:start([p1,p2]).

start(Terms) ->
  spawn(
    fun() ->
      L = [{spawn_monitor(fun() -> timer(Term) end), Term} || Term <- Terms],
      [register(T, Pid) || {{Pid, _}, T} <- L],
      receive
        {'DOWN', _Ref, process, _Pid, Atom} ->
          io:format("~p stoped.~n", [Atom]),
          start([Atom])
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


