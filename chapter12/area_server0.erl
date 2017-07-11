-module (area_server0).
-export ([loop/0]).

loop() ->
  receive
    {rectangle, Width, Height} ->
      io:format("rectangle is ~p~n", [Width * Height]),
      loop();
    {circle, Radius} ->
      io:format("circle is ~p~n", [3.14159 * Radius * Radius]),
      loop()
  end.