-module (practice4).
-export ([start_server/0, client/1]).

start_server() ->
  spawn(fun() -> server(4000) end).

server(Port) ->
  {ok, Socket} = gen_udp:open(Port, [binary]),
  io:format("server opened socket: ~p~n", [Socket]),
  loop(Socket).

loop(Socket) ->
  receive
    {udp, Socket, Host, Port, Bin} = Msg ->
      io:format("server received socket: ~p~n", [Msg]),
      {M, F, Args} = binary_to_term(Bin),
      Fac = apply(M, F, [Args]),
      gen_udp:send(Socket, Host, Port, term_to_binary(Fac)),
      loop(Socket)
  end.

% practice4:client({lists, max, [1,2,3,4]}).

client(T) ->
  {ok, Socket} = gen_udp:open(0,[binary]),
  io:format("client opened socket=~p~n", [Socket]),
  ok = gen_udp:send(Socket, "localhost", 4000, term_to_binary(T)),
  Value = receive
      {udp, Socket, _, _, Bin} = Msg ->
        io:format("client received: ~p~n", [Msg]),
        binary_to_term(Bin)
      after 2000 ->
        0
      end,
  gen_udp:close(Socket),
  Value.

