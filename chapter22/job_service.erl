-module (job_service).
-export ([test/0]).

test() ->
  _Pid = job_center2:start(),
  job_center2:add_job(fun() -> 1+1 end),
  job_center2:add_job(fun() -> 1+2 end),
  job_center2:add_job(fun() -> 1+3 end),

  W1 = worker2:recruit(),
  W2 = worker2:recruit(),
  W3 = worker2:recruit(),


  % [{pending,0},{taken,1},{completed,2}] = job_center2:stat(),

  job_center2:stop(),
  ok.

