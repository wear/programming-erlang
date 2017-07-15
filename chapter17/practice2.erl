-module (practice2).
-export ([start_nano_server/0, nano_client_eval/1]).

% practice2:start_nano_server().

start_nano_server() ->
  {ok, Listen} = gen_tcp:listen(2345, [binary,
                                      {packet, 4},
                                      {reuseaddr, true},
                                      {active, true}]),
  {ok, Socket} = gen_tcp:accept(Listen),
  gen_tcp:close(Listen),
  loop(Socket).

loop(Socket) ->
  receive
    {tcp, Socket, Bin} ->
      io:format("Server received bin = ~p~n", [Bin]),
      {M, F, Args} = binary_to_term(Bin),
      io:format("Server unpack term = ~p~n", [{M, F, Args}]),
      Reply = apply(M, F, [Args]),
      io:format("Server replying str = ~p~n", [Reply]),
      gen_tcp:send(Socket, term_to_binary(Reply)),
      loop(Socket);
    {tcp_closed, Socket} ->
      io:format("Server closed ~n")
  end.

% practice2:nano_client_eval("list_to_tuple([1+1])").

% practice2:nano_client_eval({lists, max, [1,2,3,4]}).

nano_client_eval(T) ->
  {ok, Socket} = gen_tcp:connect("localhost", 2345, [binary, {packet, 4}]),
  ok = gen_tcp:send(Socket, term_to_binary(T)),
  receive
    {tcp, Socket, Bin} ->
      io:format("Client received Bin = ~p~n", [Bin]),
      Val = binary_to_term(Bin),
      io:format("Client result = ~p~n", [Val]),
      gen_tcp:close(Socket)
  end.


