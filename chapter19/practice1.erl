-module (practice1).
-export ([test/0]).

test() ->
  Tab = ets:new(?MODULE, []),
  [ets:insert(Tab, {F,A}) || {F,A} <- erlang:module_info(exports)],
  File = filename:join(filename:dirname(code:which(?MODULE)), "./erlang.tab"),
  ets:file2tab(File).
