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
      {_,Time,_} = Job,
      spawn_link(fun()-> lazy_check(Pid, Time) end),
      From ! {self(), Pid},
      loop();
    {'DOWN', _, process, _Pid, JobNumber} ->
      job_revert(JobNumber),
      io:format("~p reverted.~n", [JobNumber]),
      loop()
  end.

-spec lazy_check(Pid, Time) -> F when
  Pid :: pid(),
  F :: fun(),
  Time :: integer().

lazy_check(Pid, Time) ->
  receive
    after
      Time - 1000 ->
        Pid ! hurry_up,
        receive
          after 2000 ->
            exit(Pid, youre_fired)
        end
  end.

test() ->
  Pid = company:start(),
  job_center3:start_link(),
  add_job(fun()-> 1+1 end),
  W1 = rpc(Pid, hire),
  worker3:rpc(W1, done),
  ok.


