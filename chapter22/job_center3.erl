-module (job_center3).
-behavior(gen_server).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export ([start_link/0, add_job/1, work_wanted/0, job_done/1, stat/0, job_revert/1]).
-export ([test/0]).

-define(StatusPending, pending).
-define(StatusTaken, taken).
-define(StatusCompleted, completed).

init([]) ->
  Tab = ets:new(?MODULE, [ordered_set]),
  ets:insert(Tab, {index, 1}),
  {ok, Tab}.

handle_call({add, F}, _From, Tab) ->
  [{_, Index}] = ets:lookup(Tab, index),
  ets:insert(Tab, [{index, Index+1}, {Index, ?StatusPending, F}]),
  {reply, Index, Tab};

handle_call(wanted, _From, Tab) ->
  case ets:match(Tab, {'$1', ?StatusPending, '$2'}) of
    [] -> {reply, no, Tab};
    [[N, F]|_T] ->
      ets:insert(Tab, {N, ?StatusTaken, F}),
      {reply, {N,F}, Tab}
  end;

handle_call({done, JobNumber}, _From, Tab) ->
  update_job(JobNumber, ?StatusCompleted, Tab),
  {reply, ok, Tab};

handle_call({revert, JobNumber}, _From, Tab) ->
  update_job(JobNumber, ?StatusPending, Tab),
  {reply, ok, Tab};

handle_call(stat, _From, Tab) ->
  Stat = tab_stat(
    Tab,
    [?StatusPending, ?StatusTaken, ?StatusCompleted],
    []),
  {reply, Stat, Tab}.


handle_cast(_Msg, Tab) -> {noreply, Tab}.
handle_info(_Info, Tab) -> {noreply, Tab}.
terminate(_Reason, _Tab) -> ok.

% Custom callback
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add_job(F) -> JobNumber when
  F :: fun(),
  JobNumber :: integer().

add_job(F) -> gen_server:call(?MODULE, {add, F}).

-spec work_wanted() -> {JobNumber, F} | no when
  JobNumber :: integer(),
  F :: fun().

work_wanted() -> gen_server:call(?MODULE, wanted).

-spec job_done(JobNumber) -> ok when
  JobNumber :: integer().

job_done(JobNumber) -> gen_server:call(?MODULE, {done, JobNumber}).

-spec job_revert(JobNumber) -> ok when
  JobNumber :: integer().

job_revert(JobNumber) -> gen_server:call(?MODULE, {revert, JobNumber}).

-spec stat() -> TupleList when
  TupleList :: [tuple()].

stat() -> gen_server:call(?MODULE, stat).


% helper
-type status() :: ?StatusPending | ?StatusTaken | ?StatusCompleted.
-type status_stat() :: {status(), integer()}.
-spec tab_stat(Tab, StatusList, Acc) -> TupleList when
  Tab :: reference(),
  StatusList :: [] | [status()],
  Acc :: [] | [status_stat()],
  TupleList :: [status_stat()].

tab_stat(_Tab, [], Acc) ->
  Acc;
tab_stat(Tab, StatusList, Acc) ->
  Status = lists:nth(1, StatusList),
  Count = length(ets:match(Tab, {'$1', Status, '_'})),
  tab_stat(
    Tab,
    lists:nthtail(1, StatusList),
    lists:append(Acc, [{Status, Count}])).

-spec update_job(JobNumber, Status, Tab) -> ok when
  JobNumber :: integer(),
  Status :: status(),
  Tab :: ets:tid().

update_job(JobNumber, Status, Tab) ->
  [{JobNumber, _, F}] = ets:lookup(Tab, JobNumber),
  ets:insert(Tab, {JobNumber, Status, F}),
  ok.
% tests
test() ->
  start_link(),
  1 = add_job(fun() -> 1+1 end),
  2 = add_job(fun() -> 1+1 end),
  3 = add_job(fun() -> 1+1 end),
  {1, _} = work_wanted(),
  {2, _} = work_wanted(),
  {3, _} = work_wanted(),
  no = work_wanted(),
  ok = job_done(1),
  [{pending,0},{taken,2},{completed,1}] = stat(),
  job_revert(2),
  [{pending,1},{taken,1},{completed,1}] = stat(),
  ok.