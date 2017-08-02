-module(job_center).
-behaviour(gen_server).

-export([start_link/0, add_job/2, work_wanted/0, job_done/1, statistics/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-define(TIME, 1000).
-record(state, {undo, doing, done, index}).
%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
add_job(F, Time) ->
  gen_server:call(?MODULE, {add_job, F, Time}).
work_wanted() ->
  gen_server:call(?MODULE, get_job).
job_done(JobNumber) ->
  gen_server:cast(?MODULE, {job_done, JobNumber}).
statistics() ->
  gen_server:call(?MODULE, statistics).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  process_flag(trap_exit, true),
  timer:send_interval(?TIME, self(), check_worker),
  {ok, #state{undo = queue:new(), doing = [], done = [], index = 1}}.

handle_call({add_job, F, Time}, _From, #state{undo = UndoPool, index = Idx} = State) ->
  State1 = State#state{undo = queue:in({Idx, Time, F}, UndoPool), index = Idx + 1},
  {reply, Idx, State1};
handle_call(get_job, {From, _}, #state{undo = UndoPool, doing = DoingPool} = State) ->
  {Reply, State1} = case queue:out(UndoPool) of
                      {{value, {Idx, Time, Fun}}, UndoPool2} ->
                        {{Idx, Time, Fun}, State#state{undo = UndoPool2, doing = [{Idx, Time, Fun, From, now_in_secs()} | DoingPool]}};
                      {empty, _} ->
                        {no, State}
                    end,
  erlang:monitor(process, From),
  {reply, Reply, State1};
handle_call(statistics, _From, #state{undo = UndoPool, doing = DoingPool, done = DonePool} = State) ->
  io:format("undo:~p~n", [UndoPool]),
  io:format("doing:~p~n", [DoingPool]),
  io:format("done:~p~n", [DonePool]),
  {reply, [{undo, queue:len(UndoPool)}, {doing, length(DoingPool)}, {done, length(DonePool)}], State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({job_done, JobNumber}, #state{doing = DoingPool, done = DonePool} = State) ->
  Item = lists:keyfind(JobNumber, 1, DoingPool),
  DoingPool2 = lists:delete(Item, DoingPool),
  {Idx, Time, Fun, From, _} = Item,
  DonePool2 = [{Idx, Time, Fun, From} | DonePool],
  {noreply, State#state{doing = DoingPool2, done = DonePool2}};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'DOWN', _Ref, process, From, normal}, #state{undo = UndoPool, doing = DoingPool} = State) ->
  State1 = case lists:keyfind(From, 4, DoingPool) of
             Item when is_tuple(Item) ->
               {Idx, Time, Fun, _From, _} = Item,
               DoingPool2 = lists:delete(Item, DoingPool),
               State#state{undo = queue:in({Idx, Time, Fun}, UndoPool), doing = DoingPool2};
             false -> State
           end,
  {noreply, State1};
handle_info(check_worker, #state{undo = UndoPool, doing = DoingPool} = State) ->
  Now = now_in_secs(),
  F = fun(Doing) ->
    {Idx, Time, Fun, From, StartTime} = Doing,
    if
      Now =:= StartTime + Time - 1 -> From ! hurry_up;
      Now =:= StartTime + Time + 1 ->
        exit(From, youre_fired),
        State#state{undo = queue:in({Idx, Time, Fun}, UndoPool)};
      true -> ok
    end
  end,
  lists:foreach(F, DoingPool),
  {noreply, State};
handle_info(Info, State) ->
  io:format("extra info:~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
now_in_secs() ->
  {A, B, _} = os:timestamp(),
  A * 1000000 + B.



-module(job_center).
-behaviour(gen_server).

-export([start_link/0, add_job/2, work_wanted/0, job_done/1, statistics/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-define(TIME, 1000).
-record(state, {undo, doing, done, index}).
%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
add_job(F, Time) ->
  gen_server:call(?MODULE, {add_job, F, Time}).
work_wanted() ->
  gen_server:call(?MODULE, get_job).
job_done(JobNumber) ->
  gen_server:cast(?MODULE, {job_done, JobNumber}).
statistics() ->
  gen_server:call(?MODULE, statistics).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  process_flag(trap_exit, true),
  timer:send_interval(?TIME, self(), check_worker),
  {ok, #state{undo = queue:new(), doing = [], done = [], index = 1}}.

handle_call({add_job, F, Time}, _From, #state{undo = UndoPool, index = Idx} = State) ->
  State1 = State#state{undo = queue:in({Idx, Time, F}, UndoPool), index = Idx + 1},
  {reply, Idx, State1};
handle_call(get_job, {From, _}, #state{undo = UndoPool, doing = DoingPool} = State) ->
  {Reply, State1} = case queue:out(UndoPool) of
                      {{value, {Idx, Time, Fun}}, UndoPool2} ->
                        {{Idx, Time, Fun}, State#state{undo = UndoPool2, doing = [{Idx, Time, Fun, From, now_in_secs()} | DoingPool]}};
                      {empty, _} ->
                        {no, State}
                    end,
  erlang:monitor(process, From),
  {reply, Reply, State1};
handle_call(statistics, _From, #state{undo = UndoPool, doing = DoingPool, done = DonePool} = State) ->
  io:format("undo:~p~n", [UndoPool]),
  io:format("doing:~p~n", [DoingPool]),
  io:format("done:~p~n", [DonePool]),
  {reply, [{undo, queue:len(UndoPool)}, {doing, length(DoingPool)}, {done, length(DonePool)}], State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({job_done, JobNumber}, #state{doing = DoingPool, done = DonePool} = State) ->
  Item = lists:keyfind(JobNumber, 1, DoingPool),
  DoingPool2 = lists:delete(Item, DoingPool),
  {Idx, Time, Fun, From, _} = Item,
  DonePool2 = [{Idx, Time, Fun, From} | DonePool],
  {noreply, State#state{doing = DoingPool2, done = DonePool2}};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'DOWN', _Ref, process, From, normal}, #state{undo = UndoPool, doing = DoingPool} = State) ->
  State1 = case lists:keyfind(From, 4, DoingPool) of
             Item when is_tuple(Item) ->
               {Idx, Time, Fun, _From, _} = Item,
               DoingPool2 = lists:delete(Item, DoingPool),
               State#state{undo = queue:in({Idx, Time, Fun}, UndoPool), doing = DoingPool2};
             false -> State
           end,
  {noreply, State1};
handle_info(check_worker, #state{undo = UndoPool, doing = DoingPool} = State) ->
  Now = now_in_secs(),
  F = fun(Doing) ->
    {Idx, Time, Fun, From, StartTime} = Doing,
    if
      Now =:= StartTime + Time - 1 -> From ! hurry_up;
      Now =:= StartTime + Time + 1 ->
        exit(From, youre_fired),
        State#state{undo = queue:in({Idx, Time, Fun}, UndoPool)};
      true -> ok
    end
  end,
  lists:foreach(F, DoingPool),
  {noreply, State};
handle_info(Info, State) ->
  io:format("extra info:~p~n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
now_in_secs() ->
  {A, B, _} = os:timestamp(),
  A * 1000000 + B.
Test:


$ erl -sname aa
Erlang/OTP 17 [erts-6.1] [source] [64-bit] [smp:4:4] [async-threads:10]

Eshell V6.1  (abort with ^G)
1> c(job_center).
{ok,job_center}
2> F1 = fun() -> 1 end.
#Fun<erl_eval.20.90072148>
3> F2 = fun() -> 2 end.
#Fun<erl_eval.20.90072148>
4> job_center:add_job(F1, 10).
1
5> job_center:add_job(F2, 10).
2
6> job_center:add_job(F1, 20).
3
7> job_center:add_job(F2, 20).
4
8> job_center:statistics().
undo:{[{4,20,#Fun<erl_eval.20.90072148>},
       {3,20,#Fun<erl_eval.20.90072148>},
       {2,10,#Fun<erl_eval.20.90072148>}],
      [{1,10,#Fun<erl_eval.20.90072148>}]}
doing:[]
done:[]
[{undo,4},{doing,0},{done,0}]
9> job_center:work_wanted().
{1,10,#Fun<erl_eval.20.90072148>}
10> job_center:job_done(1).
ok
11> job_center:statistics().
undo:{[{4,20,#Fun<erl_eval.20.90072148>},{3,20,#Fun<erl_eval.20.90072148>}],
      [{2,10,#Fun<erl_eval.20.90072148>}]}
doing:[]
done:[{1,10,#Fun<erl_eval.20.90072148>,<0.83.0>}]
[{undo,3},{doing,0},{done,1}]
12> job_center:work_wanted().
{2,10,#Fun<erl_eval.20.90072148>}
13> receive V -> V end.
hurry_up

=ERROR REPORT==== 24-Aug-2014::14:37:03 ===
** Generic server job_center terminating
** Last message in was {'EXIT',<0.83.0>,youre_fired}
** When Server state == {state,{[{4,20,#Fun<erl_eval.20.90072148>}],
                                [{3,20,#Fun<erl_eval.20.90072148>}]},
                               [{2,10,#Fun<erl_eval.20.90072148>,<0.83.0>,
                                 1408862212}],
                               [{1,10,#Fun<erl_eval.20.90072148>,<0.83.0>}],
                               5}
** Reason for termination ==
** youre_fired
** exception error: youre_fired