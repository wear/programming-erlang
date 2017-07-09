-module (type).
-spec geocode(float(), float()) -> point().
-type point() :: {float(), float()}.
-export ([geocode/2]).

geocode(X,Y) -> {X,Y}.