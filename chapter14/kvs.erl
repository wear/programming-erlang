-module (kvs).
-spec start() -> true.
-spec store(Key, Value) -> true when
  Key :: term(),
  Value :: term().
-spec lookup(Key) -> {ok, Value} when
  Key :: term(),
  Value :: term().
-export ([start/0, store/2, lookup/1]).

start() ->
  register(kvs, spawn(fun() -> loop() end)).

rpc(Q) ->
  kvs ! {self(), Q},
  receive
    {kvs, Reply} -> Reply
  end.

store(Key, Value) -> rpc({store, Key, Value}).

lookup(Key) -> rpc({lookup, Key}).

loop() ->
  receive
    {From, {store, Key, Value}} ->
      put(Key, {ok, Value}),
      From ! {kvs, true},
      loop();
    {From, {lookup, Key}} ->
      From ! {kvs, get(Key)},
      loop()
  end.
