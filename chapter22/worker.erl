-module (worker).
-export([request_job/0, apply_job/0]).

request_job() ->
  case job_center:work_wanted() of
    {JobNumber, F} ->
      io:format("Get job: ~p~n", [JobNumber]),
      try F() of
        Result -> {Result, job_center:job_done(JobNumber)}
      catch
        _:_ -> exit({JobNumber, F})
      end;
    no -> no_more_pending_jobs
  end.

apply_job() ->
  spawn(
    fun() ->
      Ref = monitor(process, spawn(fun() -> request_job() end)),
      receive
        {'DOWN', Ref, process, _Pid, normal} -> ok;
        {'DOWN', Ref, process, _Pid, {JobNumber, F}} -> job_center:job_fail({JobNumber, F})
      end
    end
  ).