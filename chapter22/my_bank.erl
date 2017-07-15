-module (my_bank).
-behavior(gen_server).
-export ([test/0]).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-export ([new_account/1,deposit/2,withdraw/2,stop/0,start/0]).
-export([lookup/2]).

start() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

new_account(Who) -> gen_server:call(?MODULE, {new, Who}).
deposit(Who, Amount) -> gen_server:call(?MODULE, {add, Who, Amount}).
withdraw(Who, Amount) -> gen_server:call(?MODULE, {remove, Who, Amount}).

init([]) -> {ok, []}.

handle_call({new, Who}, _From, State) ->
  case lookup(Who, State) of
    [] -> {reply, {welcome, Who}, [{Who, 0}|State]};
    [_] -> {reply, {Who, you_already_are_a_customer}, State}
  end;
handle_call({add, Who, Amount}, _From, State) ->
  case lookup(Who, State) of
    [] -> {reply, {Who, not_a_customer}, State};
    [{Who, Balance}] ->
      NewBalance = Balance + Amount,
      {reply, {thanks, Who, your_blance_is, NewBalance}, update({Who, NewBalance}, State)}
  end;
handle_call({remove, Who, X}, _From, State) ->
  case lookup(Who, State) of
    [] -> {reply, {Who, not_a_customer}, State};
    [{Who, Amount}] when X =< Amount ->
      NewBalance = Amount - X,
      {reply, {thanks,Who,your_blance_is,NewBalance}, update({Who, NewBalance}, State)};
    [{Who, Amount}] ->
      {reply, {sorry,Who,you_only_have,Amount, in_the_bank}, State}
  end;
handle_call(stop, _From, State) -> {stop, normal, stopped, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

lookup(_Who, []) -> [];
lookup(Who, State) ->
  [{Name, Amount} || {Name,Amount} <- State, Name =:= Who].

update({Who, Balance}, State) ->
  case lookup(Who, State) of
    [] -> not_a_customer;
    [{Who, _}] ->
      L = lists:dropwhile(fun({Name, _}) -> Name =:= Who end, State),
      [{Who, Balance} | L]
  end.

test() ->
  [] = lookup("Stephen Kong", []),
  [{"Stephen Kong", 100}] = lookup("Stephen Kong", [{"Stephen Kong", 100}]),
  [{"Stephen Kong", 200}] = update({"Stephen Kong", 200}, [{"Stephen Kong", 100}]).


% my_bank:start().
% my_bank:deposit("Stephen", 10).
% my_bank:new_account("Stephen").
% my_bank:withdraw("Stephen",100).
% my_bank:withdraw("Stephen",5).

