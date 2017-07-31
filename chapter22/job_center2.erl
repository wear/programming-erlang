-module (job_center2).
-behavior(gen_server).
-define(StatusPending, pending).
-define(StatusTaken, taken).
-define(StatusCompleted, completed).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export ([add_job/1,work_wanted/0,job_done/1,stop/0,start/0,stat/0]).

% gen_server template
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

init([]) ->
  {ok, ets:new(?MODULE, [ordered_set])}.

% gen_server callback
add_job(F) -> gen_server:call(?MODULE, {add, F}).
work_wanted() -> gen_server:call(?MODULE, {take}).
State() -> gen_server:call(?MODULE, {stat}).
job_done(JobNumber) -> gen_server:call(?MODULE, {done, JobNumber}).

handle_call({add, F}, _From, State) ->
  JobNumber = ets:info(State, size) + 1,
  ets:insert(State, {JobNumber, ?StatusPending, F}),
  {reply, JobNumber, State};

handle_call({take}, _From, State) ->
  case ets:match(State, {'$1', ?StatusPending, '$2'}) of
    [] -> {reply, no, State};
    Jobs ->
      [[N, F]|_T] = Jobs,
      ets:insert(State, {N, ?StatusTaken, F}),
      {reply, {N,F}, State}
  end;

handle_call({done, JobNumber}, _From, State) ->
  [{JobNumber, _, F}] = ets:lookup(JobNumber),
  ets:insert(State, {JobNumber, ?StatusCompleted, F}),
  {reply, {JobNumber,is_completed}, State};

handle_call({stat}, _From, State) ->
  Statistics = jobs_stat(ets:tab2list(State), [{pending, 0}, {taken, 0}, {completed, 0}]),
  {reply, Statistics, State};

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

jobs_stat([], Acc) -> Acc;
jobs_stat([H|T], Acc) ->
  {_,_,Status} = H,
  [{pending, PendingCount}, {taken, TakenCount}, {completed, CompletedCount}] = Acc,
  case Status of
    pending -> jobs_stat(T, [{pending, PendingCount+1}, {taken, TakenCount}, {completed, CompletedCount}]);
    taken -> jobs_stat(T, [{pending, PendingCount}, {taken, TakenCount+1}, {completed, CompletedCount}]);
    completed -> jobs_stat(T, [{pending, PendingCount}, {taken, TakenCount}, {completed, CompletedCount+1}])
  end.