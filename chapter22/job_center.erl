-module (job_center).
-behavior(gen_server).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-export ([add_job/1,work_wanted/0,job_done/1,stop/0,start/0]).

-export([test/0]).
-export([job_fail/1]).

start() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

add_job(F) -> gen_server:call(?MODULE, {add, F}).
work_wanted() -> gen_server:call(?MODULE, {take}).
job_done(JobNumber) -> gen_server:call(?MODULE, {done, JobNumber}).
% stat() -> gen_server:call(?MODULE, {stat}).

init([]) -> {ok, ets:new(?MODULE, [])}.

handle_call({add, F}, _From, State) ->
  JobNumber = ets:info(State, size) + 1,
  ets:insert(State, {JobNumber,F,pending}),
  {reply, JobNumber, State};

handle_call({take}, _From, State) ->
  case ets:info(State, size) of
    0 -> {reply, no, State};
    _ ->
      , [{JobNumber, F, pending}]
      case ets:lookup(JobNumber = ets:first()),
      ets:insert(State, {JobNumber, F, taken}}),
      {reply, {JobNumber,F}, State}
  end;

handle_call({done, JobNumber}, _From, State) ->
  case ets:lookup(State, JobNumber) of
    [] -> {reply, no, State};
    [Job] ->
      {JobNumber,F,taken} = Job,
      ets:insert(State, {JobNumber, F, completed}}),
      {reply, {JobNumber,is_completed}, State}
  end;

% handle_call({stat}, _From, State) ->
%   Statistics = jobs_stat(State, [{pending, 0}, {taken, 0}, {completed, 0}]),
%   {reply, {State, Statistics}, State};

handle_call(stop, _From, State) -> {stop, normal, State, State}.

% jobs_stat([], Acc) -> Acc;
% jobs_stat([H|T], Acc) ->
%   {_,_,Status} = H,
%   [{pending, PendingCount}, {taken, TakenCount}, {completed, CompletedCount}] = Acc,
%   case Status of
%     pending -> jobs_stat(T, [{pending, PendingCount+1}, {taken, TakenCount}, {completed, CompletedCount}]);
%     taken -> jobs_stat(T, [{pending, PendingCount}, {taken, TakenCount+1}, {completed, CompletedCount}]);
%     completed -> jobs_stat(T, [{pending, PendingCount}, {taken, TakenCount}, {completed, CompletedCount+1}])
%   end.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% init_jobs() ->
%   start(),
%   add_job(fun() -> 1+1 end),
%   add_job(fun() -> 1+2 end),
%   add_job(fun() -> 1+3 end).

test() ->
  [{pending, 0}, {taken, 0}, {completed, 0}] = jobs_stat([], [{pending, 0}, {taken, 0}, {completed, 0}]),
  [{pending, 1}, {taken, 0}, {completed, 0}] = jobs_stat([{"a", "b", pending}], [{pending, 0}, {taken, 0}, {completed, 0}]).


