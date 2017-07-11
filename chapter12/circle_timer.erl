-module (circle_timer).
-export ([run/2]).

run(N, M) ->
  statistics(runtime),
  statistics(wall_clock),
  L = for(1, N, fun() -> spawn(fun() -> wait() end) end),
  for(
    1,
    M,
    fun() ->
      lists:foreach(fun(Pid) -> Pid ! die end, L)
    end
  ),
  {_, Time1} = statistics(runtime),
  {_, Time2} = statistics(wall_clock),
  U1 = Time1 * 1000 / N,
  U2 = Time2 * 1000 / N,
  io:format("process spawn time=~p (~p) microsecond~n", [U1, U2]).

wait() ->
  receive
    die -> void
  end.

for(N, N, F) -> [F()];
for(I, N, F) -> [F()|for(1+I, N, F)].