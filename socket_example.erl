-module (socket_example).
-export ([nano_get_url/0, nano_get_url/1]).
-define(HOST, "www.baidu.com").

nano_get_url() ->
  nano_get_url(?HOST, "/").

nano_get_url(Path) ->
  nano_get_url(?HOST, Path).

nano_get_url(Host, Path) ->
  {ok, Socket} = gen_tcp:connect(Host, 80, [binary, {packet, 0}]),
  RequestHeader = ["GET ",Path," HTTP/1.0\r\n\r\n"],
  ok = gen_tcp:send(Socket, RequestHeader),
  receive_data(Socket, []).

receive_data(Socket, SoFar) ->
  receive
    {tcp, Socket, Bin} ->
      receive_data(Socket, [Bin|SoFar]);
    {tcp_closed, Socket} ->
      B = lists:reverse(SoFar),
      read_response(B)
  end.

read_response([H|T]) ->
  [Status|Headers] = string:split(H, "\r\n", all),
  case read_status(Status) of
    200 -> save_to_html_file(T);
    302 -> read_location(Headers)
  end.

save_to_html_file(T) ->
  file:write_file("baidu.html", T).

read_location(Headers) ->
  Location = lists:filter(
    fun(H) ->
     [A|_B] = string:split(H, ": "),
     A =:= <<"Location">>
    end,
    Headers
  ),
  [_, Url] = string:split(Location, ": "),
  [_,_,Path] = string:replace(Url, ?HOST, ""),
  nano_get_url(Path).

read_status(S) ->
  [_,B,_] = string:lexemes(S, " "),
  {Code, _} = string:to_integer(B),
  Code.