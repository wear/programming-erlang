-module (company).
-export ([start/0, rpc/2]).
-import (job_center3, [add_job/1, work_wanted/0, job_done/1, job_revert/1]).

-export ([test/0]).

start() ->
  spawn(fun() -> loop() end).

rpc(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Reponse} -> Reponse
  end.

loop() ->
  receive
    {From, hire} ->
      Job = work_wanted(),
      Pid = worker3:start(Job),
      monitor(process, Pid),
      From ! {self(), Pid},
      loop();
    {'DOWN', _, process, _Pid, JobNumber} ->
      job_revert(JobNumber),
      io:format("~p reverted.~n", [JobNumber]),
      loop()
  end.

test() ->
  Pid = company:start(),
  job_center3:start_link(),
  add_job(fun()-> a = b end),
  W1 = rpc(Pid, hire),
  worker3:rpc(W1, done),
  ok.


