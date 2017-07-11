-module (register).
-export ([start/2]).

start(Name, F) ->
  Pid = spawn(F),
  register(Name, Pid).