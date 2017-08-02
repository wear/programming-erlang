-module (worker4).
-export ([start/0,rpc/2]).
-import (job_center4, [add_job/1, work_wanted/0, job_done/1]).
-export ([test/0]).

start() ->
  spawn(fun() ->
    Job = work_wanted(),
    loop(Job)
  end).

rpc(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Reponse} -> Reponse
  end.

loop({JobNumber, _F}) ->
  receive
    {_From, done} ->
      job_done(JobNumber);
    hurry_up ->
      io:format("~p need hurry up.~n", [JobNumber])
  end.

test() ->
  job_center4:start_link(),
  job_center4:add_job(fun() -> 1+1 end),
  ok.
