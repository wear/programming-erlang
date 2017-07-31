-module (worker3).
-export ([start/1,rpc/2]).
-import (job_center3, [add_job/1, work_wanted/0, job_done/1]).
-export ([test/0]).

-type job() :: {integer(), fun()}.

-spec start(Job) -> pid() when
  Job :: job().

start(Job) -> spawn(fun() -> loop(Job) end).

rpc(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Reponse} -> Reponse
  end.

loop(Job) ->
  receive
    {From, done} ->
      {JobNumber, F} = Job,

      try F() of
        Result ->
          job_done(JobNumber),
          From ! {self(), Result}
      catch
        _:_ -> exit(JobNumber)
      end,
      done
  end.

test() ->
  job_center3:start_link(),
  add_job(fun() -> 1+1 end),
  Job = work_wanted(),
  Pid = start(Job),
  2 = rpc(Pid, done),
  ok.
