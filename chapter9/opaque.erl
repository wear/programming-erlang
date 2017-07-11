-module (opaque).
-export ([testing/0]).

testing() ->
  {A, B} = type:geocode(1.0,2.0).