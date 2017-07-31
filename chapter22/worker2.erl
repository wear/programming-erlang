-module (worker2).
-export ([recruit/0,rpc/2]).

recruit() ->
  spawn(fun() -> loop({}) end).

rpc(Pid, Request) ->
  Pid ! {Pid, Request},
  receive
    {Pid, Reply} -> Reply
  end.

loop(Job) ->
  receive
    {From, apply} ->
      {JobNumber, F} = job_center2:work_wanted(),
      io:format("Get job: ~p~n", [JobNumber]),

      loop({JobNumber, F});
    {_From, done} ->
      {JobNumber, F} = Job,
      io:format("Job result: ~p~n", [F()]),
      job_center2:job_done(JobNumber)
  end.